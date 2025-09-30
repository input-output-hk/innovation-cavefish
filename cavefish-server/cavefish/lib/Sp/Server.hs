{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Sp.Server where

import Cardano.Api qualified as Api
import Control.Concurrent.STM
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader (..))
import Core.Intent (BuildTxResult (..), IntentW, satisfies, toInternalIntent)
import Core.Proof (
  Proof,
  mkProof,
  parseHex,
  renderHex,
 )
import Core.TxAbs (TxAbs)
import Crypto.Error (CryptoFailable (..))
import Crypto.Hash (SHA256 (..), hashlazy)
import Crypto.PubKey.Ed25519 qualified as Ed
import Data.Aeson
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time
import Data.UUID
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import Servant
import Sp.App (AppM, Env (..), runApp)
import Sp.State (ClientId (ClientId), ClientRegistration (..), Pending (..))

type CavefishApi =
  "prepare" :> ReqBody '[JSON] PrepareReq :> Post '[JSON] PrepareResp
    :<|> "finalise" :> ReqBody '[JSON] FinaliseReq :> Post '[JSON] FinaliseResp
    :<|> "register" :> ReqBody '[JSON] RegisterReq :> Post '[JSON] RegisterResp
    :<|> "clients" :> Get '[JSON] ClientsResp
    :<|> "pending" :> Get '[JSON] PendingResp

-- TODO WG: Probably add an endpoint that says "Has my tx been submitted?",
-- whereupon, if it has been submitted, the response is "yes, and here it is in case you don't believe me"

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
  toJSON RegisterReq{..} =
    object ["publicKey" .= renderHex (BA.convert publicKey)]

newtype RegisterResp = RegisterResp
  { id :: UUID
  }
  deriving (Eq, Show, Generic)
instance FromJSON RegisterResp
instance ToJSON RegisterResp

cavefishApi :: Proxy CavefishApi
cavefishApi = Proxy

mkApp :: Env -> Application
mkApp env = serve cavefishApi (hoistServer cavefishApi (runApp env) server)

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
    pure PrepareReq{..}
instance ToJSON PrepareReq where
  toJSON PrepareReq{..} =
    object
      [ "intent" .= intent
      , "observer" .= renderHex observer
      , "clientId" .= clientId
      ]

data PrepareResp = PrepareResp
  { txId :: Text
  , txAbs :: TxAbs Api.ConwayEra
  , proof :: Proof
  }
  deriving (Eq, Show, Generic)
instance FromJSON PrepareResp
instance ToJSON PrepareResp

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
    pure FinaliseReq{..}
instance ToJSON FinaliseReq where
  toJSON FinaliseReq{..} =
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
server = prepareH :<|> finaliseH :<|> registerH :<|> clientsH :<|> pendingH

prepareH :: PrepareReq -> AppM PrepareResp
prepareH PrepareReq{..} = do
  Env{..} <- ask
  internalIntent <- liftIO $ either (ioError . userError . T.unpack) pure (toInternalIntent intent)

  clientKnown <- liftIO . atomically $ do
    registry <- readTVar clientRegistration
    pure (Map.member clientId registry)
  unless clientKnown $
    throwError err403{errBody = "unknown client"}

  -- TODO WG: We can't do this exactly, but it'd be nice to say at this point whether or not the observer is coherent with the intent
  -- expectedObserverBytes <-
  --   liftIO $ either (ioError . userError . T.unpack) pure (intentStakeValidatorBytes intent)

  -- when (observer /= expectedObserverBytes) $
  --   throwError err422{errBody = "observer script does not match intent"}

  BuildTxResult{tx = tx, txAbs = txAbs, mockState = builtState, changeDelta = cd} <- liftIO $ build internalIntent observer

  unless (satisfies cd internalIntent txAbs) $
    throwError err422{errBody = "transaction does not satisfy intent"}

  let txBody = Api.getTxBody tx
      txId = Api.getTxId txBody
      txIdTxt = Api.serialiseToRawBytesHexText txId
      txAbsHash :: ByteString
      txAbsHash = hashTxAbs txAbs

      proof = mkProof spSk txId txAbsHash -- Proof stuff is very placeholdery
  now <- liftIO getCurrentTime
  let expiry = addUTCTime ttl now
  liftIO . atomically $
    modifyTVar' pending (Map.insert txId (Pending tx txAbsHash expiry builtState clientId))

  pure PrepareResp{txId = txIdTxt, txAbs, proof}

-- TODO WG: Realism - I may need to serialise this canonically myself?
hashTxAbs :: (ToJSON a) => a -> ByteString
hashTxAbs = BA.convert . (hashlazy @SHA256) . encode

finaliseH :: FinaliseReq -> AppM FinaliseResp
finaliseH FinaliseReq{..} = do
  Env{..} <- ask
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
        Just Pending{..} -> do
          when (now > expiry) $ do
            liftIO . atomically $
              modifyTVar' pending (Map.delete wantedTxId)
            pure ()
          if now > expiry
            then pure $ FinaliseResp txId now (Rejected "pending expired")
            else do
              mClient <- liftIO . atomically $ do
                registry <- readTVar clientRegistration
                pure (Map.lookup creator registry)
              case mClient of
                Nothing ->
                  pure $ FinaliseResp txId now (Rejected "unknown client")
                Just ClientRegistration{publicKey} ->
                  if verifyClientSignature publicKey txAbsHash lcSig
                    then do
                      res <- liftIO (submit tx mockState)
                      case res of
                        Left reason ->
                          pure $ FinaliseResp txId now (Rejected reason)
                        Right _ -> do
                          liftIO . atomically $
                            modifyTVar' pending (Map.delete wantedTxId)
                          pure $ FinaliseResp txId now Finalised
                    else pure $ FinaliseResp txId now (Rejected "invalid client signature")

registerH :: RegisterReq -> AppM RegisterResp
registerH RegisterReq{publicKey} = do
  Env{clientRegistration} <- ask
  uuid <- liftIO nextRandom
  liftIO $ atomically $ modifyTVar' clientRegistration (Map.insert (ClientId uuid) (ClientRegistration publicKey))
  pure RegisterResp{id = uuid}

clientsH :: AppM ClientsResp
clientsH = do
  Env{clientRegistration} <- ask
  regs <- liftIO . atomically $ Map.toAscList <$> readTVar clientRegistration
  pure . ClientsResp $ fmap mkClientInfo regs
 where
  mkClientInfo :: (ClientId, ClientRegistration) -> ClientInfo
  mkClientInfo (ClientId uuid, ClientRegistration{publicKey}) =
    ClientInfo
      { clientId = uuid
      , publicKey = renderHex (BA.convert publicKey)
      }

pendingH :: AppM PendingResp
pendingH = do
  Env{pending} <- ask
  pendings <- liftIO . atomically $ Map.toAscList <$> readTVar pending
  pure . PendingResp $ fmap (uncurry mkPendingItem) pendings
 where
  mkPendingItem :: Api.TxId -> Pending -> PendingItem
  mkPendingItem txId Pending{..} =
    let ClientId creatorId = creator
     in PendingItem
          { txId = Api.serialiseToRawBytesHexText txId
          , txAbsHash = renderHex txAbsHash
          , expiresAt = expiry
          , clientId = creatorId
          }

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
