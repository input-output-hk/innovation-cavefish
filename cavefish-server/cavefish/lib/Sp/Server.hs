{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Sp.Server where

import Cardano.Api qualified as Api
import Control.Concurrent.STM
import Control.Monad (unless, when)
import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader (..))
import Core.Cbor (mkWitnessBundle, serialiseClientWitnessBundle, serialiseTx)
import Core.Intent (BuildTxResult (..), ChangeDelta, IntentW, satisfies, toInternalIntent)
import Core.PaymentProof (ProofResult (..), hashTxAbs)
import Core.Pke (ciphertextDigest, decrypt, encrypt, renderError)
import Core.Proof (mkProof, parseHex, renderHex)
import Core.TxAbs (TxAbs)
import Crypto.Error (CryptoFailable (..))
import Crypto.PubKey.Ed25519 qualified as Ed
import Crypto.Random (getRandomBytes)
import Data.Aeson
import Data.Bifunctor (first)
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time
import Data.UUID
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import Ledger.Tx.CardanoAPI (CardanoTx, pattern CardanoEmulatorEraTx)
import Servant
import Sp.App (AppM, Env (..), runApp)
import Sp.State (ClientId (ClientId), ClientRegistration (..), Completed (..), Pending (..))

type CavefishApi =
  "prepare" :> ReqBody '[JSON] PrepareReq :> Post '[JSON] PrepareResp
    {- The expected flow we require (after `prepare`):
        Signer (LC)                     Service Provider (SP)
        ----------------------------------------------------------------
        WBPS Execution for m := tx||auxnt        Produce commitment comtx
                                      (comtx, TxAbs) - PrepareResp
                                      <-----------
        Produce blind sig. com. `R = g^r`
                                            R - CommitReq
                                      ----------->
                                                Produce challenge `c` and proof `π`
                                          (c, π) - CommitResp
                                      <-----------
                Check proof `π`
                                            s - FinaliseReq
                Produce `s = r + cx`  -----------> Produce signature `σ = (R, s)`

      The final shape will be something like:

      "prepare" :: PrepareReq -> (comtx, TxAbs)
      "commit" :: R -> (c, π)
      "finalise" :: s -> FinaliseResp
    -}
    :<|> "commit" :> ReqBody '[JSON] CommitReq :> Post '[JSON] CommitResp
    :<|> "finalise" :> ReqBody '[JSON] FinaliseReq :> Post '[JSON] FinaliseResp
    :<|> "register" :> ReqBody '[JSON] RegisterReq :> Post '[JSON] RegisterResp
    :<|> "clients" :> Get '[JSON] ClientsResp
    :<|> "pending" :> Get '[JSON] PendingResp
    :<|> "transaction" :> Capture "id" Text :> Get '[JSON] TransactionResp

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
  , publicKey :: Text
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
  { publicKey :: Ed.PublicKey
  }
  deriving (Eq, Show, Generic)

instance FromJSON RegisterReq where
  parseJSON = withObject "RegisterReq" $ \o -> do
    pkHex :: Text <- o .: "publicKey"
    bytes <- parseHex pkHex
    case Ed.publicKey bytes of
      CryptoFailed err -> fail (show err)
      CryptoPassed pk -> pure (RegisterReq pk)

instance ToJSON RegisterReq where
  toJSON RegisterReq {..} =
    object ["publicKey" .= renderHex (BA.convert publicKey)]

data RegisterResp = RegisterResp
  { id :: UUID
  , spPk :: Ed.PublicKey
  }
  deriving (Eq, Show, Generic)

instance FromJSON RegisterResp

instance ToJSON RegisterResp

cavefishApi :: Proxy CavefishApi
cavefishApi = Proxy

mkApp :: Env -> Application
mkApp env = serve cavefishApi (hoistServer cavefishApi (runApp env) server)

-- | Request to prepare a transaction.
data PrepareReq = PrepareReq
  { intent :: IntentW
  , observer :: ByteString
  , clientId :: ClientId
  }
  deriving (Eq, Show, Generic)

instance FromJSON PrepareReq where
  parseJSON = withObject "PrepareReq" $ \o -> do
    intent <- o .: "intent"
    obsHex :: Text <- o .: "observer"
    observer <- parseHex obsHex
    clientId <- o .: "clientId"
    pure PrepareReq {..}

instance ToJSON PrepareReq where
  toJSON PrepareReq {..} =
    object
      [ "intent" .= intent
      , "observer" .= renderHex observer
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

server :: ServerT CavefishApi AppM
server = prepareH :<|> commitH :<|> finaliseH :<|> registerH :<|> clientsH :<|> pendingH :<|> transactionH

prepareH :: PrepareReq -> AppM PrepareResp
prepareH PrepareReq {..} = do
  Env {..} <- ask
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
  case Api.deserialiseFromRawBytesHex @Api.TxId (TE.encodeUtf8 txId) of
    Left _ ->
      throwError err400 {errBody = "malformed tx id"}
    Right wantedTxId -> do
      mp <- liftIO . atomically $ do
        m <- readTVar pending
        pure (Map.lookup wantedTxId m)
      case mp of
        Nothing ->
          throwError err404 {errBody = "unknown or expired tx"}
        Just pendingEntry@Pending {..} -> do
          when (now > expiry) $ do
            liftIO . atomically $
              modifyTVar' pending (Map.delete wantedTxId) -- TODO WG: Possible an unfriendly way to handle this
            throwError err410 {errBody = "pending expired"}
          _ <- either throwError pure (decryptPendingPayload env pendingEntry)
          mClient <- liftIO . atomically $ do
            registry <- readTVar clientRegistration
            pure (Map.lookup creator registry)
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
  case Api.deserialiseFromRawBytesHex @Api.TxId (TE.encodeUtf8 txId) of
    Left _ ->
      pure $ FinaliseResp txId now (Rejected "malformed tx id")
    Right wantedTxId -> do
      mp <- liftIO . atomically $ do
        m <- readTVar pending
        pure (Map.lookup wantedTxId m)
      case mp of
        Nothing ->
          pure $ FinaliseResp txId now (Rejected "unknown or expired tx")
        Just pendingEntry@Pending {..} -> do
          when (now > expiry) $ do
            liftIO . atomically $
              modifyTVar' pending (Map.delete wantedTxId) -- TODO WG: Possible an unfriendly way to handle this
            pure ()
          if now > expiry
            then pure $ FinaliseResp txId now (Rejected "pending expired")
            else do
              unless
                (isJust commitment)
                (throwError err410 {errBody = "commitment must be made before submission"})
              _payload <- either throwError pure (decryptPendingPayload env pendingEntry)
              mClient <- liftIO . atomically $ do
                registry <- readTVar clientRegistration
                pure (Map.lookup creator registry)
              case mClient of
                Nothing ->
                  pure $ FinaliseResp txId now (Rejected "unknown client")
                Just ClientRegistration {publicKey} ->
                  if verifyClientSignature publicKey txAbsHash lcSig
                    then do
                      res <- liftIO (submit tx mockState)
                      case res of
                        Left reason ->
                          pure $ FinaliseResp txId now (Rejected reason)
                        Right _ -> do
                          let completed = Completed {tx, submittedAt = now, creator}
                          liftIO . atomically $ do
                            modifyTVar' pending (Map.delete wantedTxId) -- TODO WG: Possible an unfriendly way to handle this
                            modifyTVar' complete (Map.insert wantedTxId completed)
                          pure $ FinaliseResp txId now Finalised
                    else pure $ FinaliseResp txId now (Rejected "invalid client signature")

registerH :: RegisterReq -> AppM RegisterResp
registerH RegisterReq {publicKey} = do
  Env {clientRegistration, spSk} <- ask
  uuid <- liftIO nextRandom
  liftIO $
    atomically $
      modifyTVar' clientRegistration (Map.insert (ClientId uuid) (ClientRegistration publicKey))
  pure RegisterResp {id = uuid, spPk = Ed.toPublic spSk}

clientsH :: AppM ClientsResp
clientsH = do
  Env {clientRegistration} <- ask
  regs <- liftIO . atomically $ Map.toAscList <$> readTVar clientRegistration
  pure . ClientsResp $ fmap mkClientInfo regs
  where
    mkClientInfo :: (ClientId, ClientRegistration) -> ClientInfo
    mkClientInfo (ClientId uuid, ClientRegistration {publicKey}) =
      ClientInfo
        { clientId = uuid
        , publicKey = renderHex (BA.convert publicKey)
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
  txId <-
    case Api.deserialiseFromRawBytesHex @Api.TxId (TE.encodeUtf8 txIdText) of
      Left _ -> throwError err400 {errBody = "malformed tx id"}
      Right parsedTxId -> pure parsedTxId
  completes <- liftIO (readTVarIO complete)
  case Map.lookup txId completes of
    Just Completed {tx, submittedAt, creator = ClientId creatorId} ->
      pure $ TransactionSubmitted (SubmittedSummary (CardanoEmulatorEraTx tx) submittedAt creatorId)
    Nothing -> do
      pendings <- liftIO (readTVarIO pending)
      case Map.lookup txId pendings of
        Just Pending {expiry, creator = ClientId creatorId} ->
          pure $ TransactionPending (PendingSummary expiry creatorId)
        Nothing -> pure TransactionMissing

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
