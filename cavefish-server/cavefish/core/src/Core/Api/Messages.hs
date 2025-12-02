{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Core.Api.Messages where

import Cardano.Api qualified as Api
import Control.Concurrent.STM (TVar, atomically, modifyTVar', readTVar, readTVarIO)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader (ask))
import Core.Api.AppContext (
  AppM,
  Env (
    Env,
    build,
    clientRegistration,
    complete,
    pending,
    pkePublic,
    pkeSecret,
    spSk,
    submit,
    ttl,
    wbpsScheme
  ),
 )
import Core.Api.State (
  ClientId (ClientId),
  ClientRegistration (ClientRegistration, userPublicKey),
  Completed (Completed, creator, submittedAt, tx),
  Pending (
    Pending,
    auxNonce,
    ciphertext,
    commitment,
    creator,
    expiry,
    tx,
    txAbsHash
  ),
  renderPublicKey,
 )
import Core.Cbor (serialiseTx)
import Core.PaymentProof (ProofResult (ProofEd25519))
import Core.Pke (
  ciphertextDigest,
  decrypt,
  renderError,
 )
import Core.Proof (mkProof, renderHex)
import Crypto.Error (CryptoFailable (CryptoFailed, CryptoPassed))
import Crypto.PubKey.Ed25519 qualified as Ed
import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toJSON),
  Value (String),
  object,
  withObject,
  (.:),
  (.=),
 )
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Ledger.Tx.CardanoAPI (CardanoTx, pattern CardanoEmulatorEraTx)
import Servant (
  ServerError,
  err400,
  err403,
  err404,
  err409,
  err410,
  err500,
  errBody,
  throwError,
 )

-- | Cavefish API a
data TransactionResp
  = TransactionMissing
  | TransactionPending PendingSummary
  | TransactionSubmitted SubmittedSummary
  deriving (Eq, Show, Generic)

data PendingSummary = PendingSummary
  { pendingExpiresAt :: UTCTime
  , pendingClientId :: UUID
  }
  deriving (Eq, Show, Generic)

data SubmittedSummary = SubmittedSummary
  { submittedTx :: CardanoTx
  , submittedAt :: UTCTime
  , submittedClientId :: UUID
  }
  deriving (Eq, Show, Generic)

instance ToJSON TransactionResp where
  toJSON = \case
    TransactionMissing ->
      object ["status" .= String "missing"]
    TransactionPending PendingSummary {..} ->
      object
        [ "status" .= String "pending"
        , "expiresAt" .= pendingExpiresAt
        , "clientId" .= pendingClientId
        ]
    TransactionSubmitted SubmittedSummary {..} ->
      object
        [ "status" .= String "submitted"
        , "transaction" .= submittedTx
        , "submittedAt" .= submittedAt
        , "clientId" .= submittedClientId
        ]

instance FromJSON TransactionResp where
  parseJSON = withObject "TransactionResp" $ \o -> do
    status :: Text <- o .: "status"
    case status of
      "missing" -> pure TransactionMissing
      "pending" -> do
        expiresAt <- o .: "expiresAt"
        clientId <- o .: "clientId"
        pure $ TransactionPending (PendingSummary expiresAt clientId)
      "submitted" -> do
        tx <- o .: "transaction"
        submittedAt <- o .: "submittedAt"
        clientId <- o .: "clientId"
        pure $ TransactionSubmitted (SubmittedSummary tx submittedAt clientId)
      _ -> fail "unknown transaction status"

newtype ClientsResp = ClientsResp
  { clients :: [ClientInfo]
  }
  deriving (Eq, Show, Generic)

instance ToJSON ClientsResp

instance FromJSON ClientsResp

data ClientInfo = ClientInfo
  { clientId :: UUID
  , userPublicKey :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON ClientInfo

instance FromJSON ClientInfo

newtype PendingResp = PendingResp
  { pending :: [PendingItem]
  }
  deriving (Eq, Show, Generic)

instance ToJSON PendingResp

instance FromJSON PendingResp

data PendingItem = PendingItem
  { txId :: Text
  , txAbsHash :: Text
  , expiresAt :: UTCTime
  , clientId :: UUID
  }
  deriving (Eq, Show, Generic)

instance ToJSON PendingItem

instance FromJSON PendingItem

data CommitReq = CommitReq {txId :: Text, bigR :: Ed.PublicKey} deriving (Eq, Show, Generic)

instance FromJSON CommitReq

instance ToJSON CommitReq

data CommitResp = CommitResp {pi :: ProofResult, c :: Int} deriving (Eq, Show, Generic)

instance FromJSON CommitResp

instance ToJSON CommitResp

-- TODO WG: Once the crypto machinery lands, we can generate the pair `(c, π)`
--          and plug this handler into the middle of the demonstrateCommitment/finalise flow.
commitH :: CommitReq -> AppM CommitResp
commitH CommitReq {..} = do
  env@Env {..} <- ask
  now <- liftIO getCurrentTime
  case parseTxIdHex txId of
    Nothing ->
      throwError err400 {errBody = "malformed tx id"}
    Just wantedTxId -> do
      mp <- lookupPendingEntry pending wantedTxId
      case mp of
        Nothing ->
          throwError err404 {errBody = "unknown or expired tx"}
        Just pendingEntry@Pending {expiry, creator, commitment, ciphertext, txAbsHash, tx} -> do
          when (now > expiry) $ do
            removePendingEntry pending wantedTxId
            throwError err410 {errBody = "pending expired"}
          _ <- either throwError pure (decryptPendingPayload env pendingEntry)
          mClient <- lookupClientRegistration clientRegistration creator
          case mClient of
            Nothing ->
              throwError err403 {errBody = "unknown client"}
            Just _ ->
              case commitment of
                Just _ ->
                  throwError err409 {errBody = "commitment already recorded"}
                Nothing -> do
                  liftIO . atomically $
                    modifyTVar' pending (Map.adjust (\p -> p {commitment = Just bigR}) wantedTxId)
                  let txIdVal = Api.getTxId (Api.getTxBody tx)
                      commitmentBytes = ciphertextDigest ciphertext
                      proof = ProofEd25519 (mkProof spSk txIdVal txAbsHash commitmentBytes)
                  pure CommitResp {pi = proof, c = 0}

clientsH :: AppM ClientsResp
clientsH = do
  Env {clientRegistration} <- ask
  regs <- liftIO . atomically $ Map.toAscList <$> readTVar clientRegistration
  pure . ClientsResp $ fmap mkClientInfo regs
  where
    mkClientInfo :: (ClientId, ClientRegistration) -> ClientInfo
    mkClientInfo (ClientId uuid, ClientRegistration {userPublicKey}) =
      ClientInfo
        { clientId = uuid
        , userPublicKey = renderHex (renderPublicKey userPublicKey)
        }

pendingH :: AppM PendingResp
pendingH = do
  Env {pending} <- ask
  pendings <- liftIO . atomically $ Map.toAscList <$> readTVar pending
  pure . PendingResp $ fmap (uncurry mkPendingItem) pendings
  where
    mkPendingItem :: Api.TxId -> Pending -> PendingItem
    mkPendingItem txId Pending {creator, txAbsHash, expiry} =
      let ClientId creatorId = creator
       in PendingItem
            { txId = Api.serialiseToRawBytesHexText txId
            , txAbsHash = renderHex txAbsHash
            , expiresAt = expiry
            , clientId = creatorId
            }

transactionH :: Text -> AppM TransactionResp
transactionH txIdText = do
  Env {complete, pending} <- ask
  case parseTxIdHex txIdText of
    Nothing -> throwError err400 {errBody = "malformed tx id"}
    Just txId -> do
      completes <- Map.lookup txId <$> liftIO (readTVarIO complete)
      pendings <- lookupPendingEntry pending txId
      let toSubmitted Completed {tx, submittedAt, creator = ClientId creatorId} =
            TransactionSubmitted (SubmittedSummary (CardanoEmulatorEraTx tx) submittedAt creatorId)
          toPending Pending {expiry, creator = ClientId creatorId} =
            TransactionPending (PendingSummary expiry creatorId)
      let fallback = maybe TransactionMissing toPending pendings
      pure $ maybe fallback toSubmitted completes

finaliseSigTag :: ByteString
finaliseSigTag = "cavefish/finalise/v1"

clientSignatureMessage :: ByteString -> ByteString
clientSignatureMessage txAbsHash = finaliseSigTag <> txAbsHash

verifyClientSignature :: Ed.PublicKey -> ByteString -> ByteString -> Bool
verifyClientSignature pk txAbsHash sigBytes =
  case Ed.signature sigBytes of
    CryptoFailed _ -> False
    CryptoPassed sig ->
      let message = clientSignatureMessage txAbsHash
       in Ed.verify pk message sig

decryptPendingPayload :: Env -> Pending -> Either ServerError ByteString
decryptPendingPayload Env {pkeSecret} Pending {ciphertext = pendingCiphertext, auxNonce = pendingAuxNonce, tx} =
  case decrypt pkeSecret pendingCiphertext of
    Left err ->
      let msg = "pke decrypt failed: " <> renderError err
       in Left err500 {errBody = BL.fromStrict (TE.encodeUtf8 msg)}
    Right payload ->
      let txBytes = serialiseTx tx
          (payloadTxBytes, payloadNonce) = BS.splitAt (BS.length txBytes) payload
       in if payloadTxBytes /= txBytes
            then
              Left
                err500
                  { errBody =
                      "ciphertext payload mismatch: transaction bytes differ"
                  }
            else
              if payloadNonce /= pendingAuxNonce
                then
                  Left
                    err500
                      { errBody =
                          "ciphertext payload mismatch: aux nonce differ"
                      }
                else Right payload

parseTxIdHex :: Text -> Maybe Api.TxId
parseTxIdHex =
  either (const Nothing) Just
    . Api.deserialiseFromRawBytesHex @Api.TxId
    . TE.encodeUtf8

lookupPendingEntry :: TVar (Map Api.TxId Pending) -> Api.TxId -> AppM (Maybe Pending)
lookupPendingEntry store txId =
  liftIO . atomically $ do
    entries <- readTVar store
    pure (Map.lookup txId entries)

lookupClientRegistration ::
  TVar (Map ClientId ClientRegistration) -> ClientId -> AppM (Maybe ClientRegistration)
lookupClientRegistration store clientId =
  liftIO . atomically $ do
    registry <- readTVar store
    pure (Map.lookup clientId registry)

removePendingEntry :: TVar (Map Api.TxId Pending) -> Api.TxId -> AppM ()
removePendingEntry store txId =
  liftIO . atomically $ modifyTVar' store (Map.delete txId)
