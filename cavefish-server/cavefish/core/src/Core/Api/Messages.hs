{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Core.Api.Messages where

import Cardano.Api qualified as Api
import Cardano.Crypto.DSIGN (rawDeserialiseVerKeyDSIGN)
import Cardano.Crypto.DSIGN.Ed25519 (Ed25519DSIGN)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', readTVar, readTVarIO)
import Control.Monad (unless, when)
import Control.Monad.Except (liftEither)
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
  ClientRegistration (ClientRegistration, signerPublicKey),
  Completed (Completed, creator, submittedAt, tx),
  Pending (
    Pending,
    auxNonce,
    challenge,
    ciphertext,
    commitment,
    creator,
    expiry,
    mockState,
    rho,
    tx,
    txAbsHash
  ),
  parsePublicKey,
  renderPublicKey,
 )
import Core.Cbor (mkWitnessBundle, serialiseClientWitnessBundle, serialiseTx)
import Core.Intent (
  BuildTxResult (BuildTxResult, changeDelta, mockState, tx, txAbs),
  ChangeDelta,
  IntentW,
  satisfies,
  toInternalIntent,
 )
import Core.PaymentProof (ProofResult (ProofEd25519), hashTxAbs)
import Core.Pke (
  ciphertextDigest,
  decrypt,
  encrypt,
  renderError,
 )
import Core.Proof (mkProof, parseHex, renderHex)
import Core.TxAbs (TxAbs)
import Crypto.Error (CryptoFailable (CryptoFailed, CryptoPassed))
import Crypto.PubKey.Ed25519 qualified as Ed
import Crypto.Random (getRandomBytes)
import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toJSON),
  Value (String),
  object,
  withObject,
  (.:),
  (.=),
 )
import Data.Bifunctor (first)
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import Ledger.Tx.CardanoAPI (CardanoTx, pattern CardanoEmulatorEraTx)
import Servant (
  ServerError,
  err400,
  err403,
  err404,
  err409,
  err410,
  err422,
  err500,
  errBody,
  throwError,
 )
import WBPS qualified
import WBPS.Adapter.CardanoCryptoClass.Crypto qualified as W
import WBPS.Core (SignerKey)

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
  , signerPublicKey :: Text
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

newtype RegisterReq = RegisterReq
  { signerPublicKey :: Ed.PublicKey
  }
  deriving (Eq, Show, Generic)

instance FromJSON RegisterReq where
  parseJSON = withObject "RegisterReq" $ \o -> do
    signerPublicKey <- parsePublicKey =<< o .: "signerPublicKey"
    pure RegisterReq {signerPublicKey}

instance ToJSON RegisterReq where
  toJSON RegisterReq {..} =
    object
      [ "signerPublicKey" .= renderHex (renderPublicKey signerPublicKey)
      ]

data RegisterResp = RegisterResp
  { id :: UUID
  , spPk :: Ed.PublicKey
  , verificationContext :: Value
  }
  deriving (Eq, Show, Generic)

instance ToJSON RegisterResp where
  toJSON RegisterResp {id, spPk, verificationContext} =
    object
      [ "id" .= id
      , "spPk" .= renderHex (BA.convert spPk)
      , "verificationContext" .= verificationContext
      ]

instance FromJSON RegisterResp where
  parseJSON = withObject "RegisterResp" $ \o -> do
    id <- o .: "id"
    spHex :: Text <- o .: "spPk"
    bytes <- parseHex spHex
    verificationContext <- o .: "verificationContext"
    case Ed.publicKey bytes of
      CryptoFailed err -> fail ("invalid service public key: " <> show err)
      CryptoPassed pk -> pure RegisterResp {spPk = pk, id, verificationContext}

-- | Request to prepare a transaction.
data PrepareReq = PrepareReq
  { intent :: IntentW
  , observer :: Maybe ByteString
  , clientId :: ClientId
  }
  deriving (Eq, Show, Generic)

instance FromJSON PrepareReq where
  parseJSON = withObject "PrepareReq" $ \o -> do
    intent <- o .: "intent"
    obsHex :: Maybe Text <- o .: "observer"
    observer <- traverse parseHex obsHex
    clientId <- o .: "clientId"
    pure PrepareReq {..}

instance ToJSON PrepareReq where
  toJSON PrepareReq {..} =
    object
      [ "intent" .= intent
      , "observer" .= fmap renderHex observer
      , "clientId" .= clientId
      ]

data PrepareResp = PrepareResp
  { txId :: Text
  , txAbs :: TxAbs Api.ConwayEra
  , -- We need to include a `consumed - produced` here, otherwise the client can't run `satisfies` for `ChangeTo`
    changeDelta :: ChangeDelta
  , witnessBundleHex :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON PrepareResp

instance ToJSON PrepareResp

data CommitReq = CommitReq {txId :: Text, bigR :: Ed.PublicKey} deriving (Eq, Show, Generic)

instance FromJSON CommitReq

instance ToJSON CommitReq

data CommitResp = CommitResp {pi :: ProofResult, c :: Int} deriving (Eq, Show, Generic)

instance FromJSON CommitResp

instance ToJSON CommitResp

data FinaliseReq = FinaliseReq
  { txId :: Text
  , lcSig :: ByteString
  }
  deriving (Eq, Show, Generic)

instance FromJSON FinaliseReq where
  parseJSON = withObject "FinaliseReq" $ \o -> do
    txId <- o .: "txId"
    sigHex :: Text <- o .: "lcSig"
    lcSig <- parseHex sigHex
    pure FinaliseReq {..}

instance ToJSON FinaliseReq where
  toJSON FinaliseReq {..} =
    object
      [ "txId" .= txId
      , "lcSig" .= renderHex lcSig
      ]

data FinaliseResult
  = Finalised
  | Rejected Text
  deriving (Eq, Show, Generic)

instance ToJSON FinaliseResult

instance FromJSON FinaliseResult

data FinaliseResp = FinaliseResp
  { txId :: Text
  , submittedAt :: UTCTime
  , result :: FinaliseResult
  }
  deriving (Eq, Show, Generic)

instance ToJSON FinaliseResp

instance FromJSON FinaliseResp

prepareH :: PrepareReq -> AppM PrepareResp
prepareH PrepareReq {..} = do
  Env {pending, clientRegistration, ttl, pkePublic, build} <- ask
  internalIntent <- liftIO $ either (ioError . userError . T.unpack) pure (toInternalIntent intent)

  clientKnown <- liftIO . atomically $ do
    registry <- readTVar clientRegistration
    pure (Map.member clientId registry)
  unless clientKnown $
    throwError err403 {errBody = "unknown client"}

  -- TODO WG: We can't do this exactly, but it'd be nice to say at this point whether or not the observer is coherent with the intent
  -- expectedObserverBytes <-
  --   liftIO $ either (ioError . userError . T.unpack) pure (intentStakeValidatorBytes intent)

  -- when (observer /= expectedObserverBytes) $
  --   throwError err422{errBody = "observer script does not match intent"}

  BuildTxResult {tx = tx, txAbs = txAbs, mockState = builtState, changeDelta = cd} <-
    liftIO $ build internalIntent observer

  auxNonceBytes :: ByteString <- liftIO $ getRandomBytes 32
  rhoBytes :: ByteString <- liftIO $ getRandomBytes 32

  let payload = serialiseTx tx <> auxNonceBytes
      toServerErr msg = err500 {errBody = BL.fromStrict (TE.encodeUtf8 msg)}
      toPkeErr err = err500 {errBody = BL.fromStrict (TE.encodeUtf8 ("pke encryption failed: " <> renderError err))}
  -- The part from the paper: C ← PKE.Enc(ek, m; ρ) with ek = pkePublic, m = serialiseTx tx <> auxNonceBytes
  ciphertext <- liftEither $ first toPkeErr (encrypt pkePublic payload rhoBytes)
  witnessBundle <-
    liftEither $ first toServerErr $ mkWitnessBundle tx txAbs observer auxNonceBytes ciphertext
  let witnessBundleHex = renderHex (serialiseClientWitnessBundle witnessBundle)

  unless (satisfies cd internalIntent txAbs) $
    throwError err422 {errBody = "transaction does not satisfy intent"}

  let txBody = Api.getTxBody tx
      txId = Api.getTxId txBody
      txIdTxt = Api.serialiseToRawBytesHexText txId
      txAbsHash :: ByteString
      txAbsHash = hashTxAbs txAbs
  now <- liftIO getCurrentTime
  let expiry = addUTCTime ttl now
  liftIO . atomically $
    modifyTVar' pending $
      Map.insert
        txId
        Pending
          { tx
          , txAbsHash
          , expiry
          , mockState = builtState
          , creator = clientId
          , ciphertext
          , auxNonce = auxNonceBytes
          , rho = rhoBytes
          , commitment = Nothing
          , challenge = Nothing
          }

  pure
    PrepareResp
      { txId = txIdTxt
      , txAbs
      , changeDelta = cd
      , witnessBundleHex
      }

-- TODO WG: Once the crypto machinery lands, we can generate the pair `(c, π)`
--          and plug this handler into the middle of the prepare/finalise flow.
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
        Just pendingEntry@Pending {..} -> do
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

finaliseH :: FinaliseReq -> AppM FinaliseResp
finaliseH FinaliseReq {..} = do
  env@Env {..} <- ask
  now <- liftIO getCurrentTime
  case parseTxIdHex txId of
    Nothing ->
      pure $ FinaliseResp txId now (Rejected "malformed tx id")
    Just wantedTxId -> do
      mp <- lookupPendingEntry pending wantedTxId
      case mp of
        Nothing ->
          pure $ FinaliseResp txId now (Rejected "unknown or expired tx")
        Just pendingEntry@Pending {..}
          | now > expiry -> do
              removePendingEntry pending wantedTxId
              pure $ FinaliseResp txId now (Rejected "pending expired")
          | otherwise -> do
              unless
                (isJust commitment)
                (throwError err410 {errBody = "commitment must be made before submission"})
              _payload <- either throwError pure (decryptPendingPayload env pendingEntry)
              mClient <- lookupClientRegistration clientRegistration creator
              case mClient of
                Nothing ->
                  pure $ FinaliseResp txId now (Rejected "unknown client")
                Just ClientRegistration {signerPublicKey} ->
                  if verifyClientSignature signerPublicKey txAbsHash lcSig
                    then do
                      res <- liftIO (submit tx mockState)
                      case res of
                        Left reason ->
                          pure $ FinaliseResp txId now (Rejected reason)
                        Right _ -> do
                          let completed = Completed {tx, submittedAt = now, creator}
                          liftIO . atomically $ do
                            modifyTVar' pending (Map.delete wantedTxId)
                            modifyTVar' complete (Map.insert wantedTxId completed)
                          pure $ FinaliseResp txId now Finalised
                    else pure $ FinaliseResp txId now (Rejected "invalid client signature")

registerH :: RegisterReq -> AppM RegisterResp
registerH RegisterReq {signerPublicKey} = do
  Env {clientRegistration, spSk, wbpsScheme} <- ask
  signerKey <-
    maybe
      (throwError err422 {errBody = "invalid signer public key"})
      pure
      (mkSignerKey signerPublicKey)
  result <- liftIO $ WBPS.withFileSchemeIO wbpsScheme (WBPS.getVerificationContext signerKey)
  verificationValue <-
    either
      ( \failures -> throwError err500 {errBody = BL8.pack ("verification context unavailable: " <> show failures)}
      )
      pure
      result
  uuid <- liftIO nextRandom
  liftIO . atomically $
    modifyTVar' clientRegistration (Map.insert (ClientId uuid) (ClientRegistration signerPublicKey))
  pure RegisterResp {id = uuid, spPk = Ed.toPublic spSk, verificationContext = verificationValue}

clientsH :: AppM ClientsResp
clientsH = do
  Env {clientRegistration} <- ask
  regs <- liftIO . atomically $ Map.toAscList <$> readTVar clientRegistration
  pure . ClientsResp $ fmap mkClientInfo regs
  where
    mkClientInfo :: (ClientId, ClientRegistration) -> ClientInfo
    mkClientInfo (ClientId uuid, ClientRegistration {signerPublicKey}) =
      ClientInfo
        { clientId = uuid
        , signerPublicKey = renderHex (renderPublicKey signerPublicKey)
        }

pendingH :: AppM PendingResp
pendingH = do
  Env {pending} <- ask
  pendings <- liftIO . atomically $ Map.toAscList <$> readTVar pending
  pure . PendingResp $ fmap (uncurry mkPendingItem) pendings
  where
    mkPendingItem :: Api.TxId -> Pending -> PendingItem
    mkPendingItem txId Pending {..} =
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

mkSignerKey :: Ed.PublicKey -> Maybe SignerKey
mkSignerKey pk =
  W.PublicKey <$> rawDeserialiseVerKeyDSIGN (BA.convert pk)

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
