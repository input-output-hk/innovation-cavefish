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
import Core.Intent (BuildTxResult (..), Env (..), IntentW, satisfies, toInternalIntent)
import Core.Proof (
  Proof,
  mkProof,
  parseHex,
  renderHex,
 )
import Core.TxAbs (TxAbs)
import Crypto.Hash (SHA256 (..), hashlazy)
import Data.Aeson
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time
import GHC.Generics (Generic)
import Servant
import Sp.State (Pending (..))

type CavefishApi =
  "prepare" :> ReqBody '[JSON] PrepareReq :> Post '[JSON] PrepareResp
    :<|> "finalise" :> ReqBody '[JSON] FinaliseReq :> Post '[JSON] FinaliseResp

{- TODO WG: For realism, we'll need another endpoint: "register", which allows
            an LC to add its public key to a registry (which we'll add to `Env`)

            Currently, we can't verify a signature coming back from the LC.
            We'll need to be able to do that once we have "proper" proving in place.
-}

cavefishApi :: Proxy CavefishApi
cavefishApi = Proxy

mkApp :: Env -> Application
mkApp env = serve cavefishApi (server env)

data PrepareReq = PrepareReq
  { intent :: IntentW
  , observer :: ByteString
  }
  deriving (Eq, Show, Generic)

instance FromJSON PrepareReq where
  parseJSON = withObject "FinaliseReq" $ \o -> do
    intent <- o .: "intent"
    obsHex :: Text <- o .: "observer"
    observer <- parseHex obsHex
    pure PrepareReq{..}
instance ToJSON PrepareReq where
  toJSON PrepareReq{..} =
    object
      [ "intent" .= intent
      , "observer" .= renderHex observer
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

server :: Env -> Server CavefishApi
server env = prepareH env :<|> finaliseH env

prepareH :: Env -> PrepareReq -> Handler PrepareResp
prepareH env@Env{..} PrepareReq{..} = do
  intent <- liftIO $ either (ioError . userError . T.unpack) pure (toInternalIntent intent)

  -- TODO WG: We can't do this exactly, but it'd be nice to say at this point whether or not the observer is coherent with the intent
  -- expectedObserverBytes <-
  --   liftIO $ either (ioError . userError . T.unpack) pure (intentStakeValidatorBytes intent)

  -- when (observer /= expectedObserverBytes) $
  --   throwError err422{errBody = "observer script does not match intent"}

  BuildTxResult{tx = tx, txAbs = txAbs, mockState = builtState, changeDelta = cd} <- liftIO $ build intent observer

  unless (satisfies env cd intent txAbs) $
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
    modifyTVar' pending (Map.insert txId (Pending tx txAbsHash expiry builtState))

  pure PrepareResp{txId = txIdTxt, txAbs, proof}

-- TODO WG: Realism - I may need to serialise this canonically myself?
hashTxAbs :: (ToJSON a) => a -> ByteString
hashTxAbs = BA.convert . (hashlazy @SHA256) . encode

finaliseH :: Env -> FinaliseReq -> Handler FinaliseResp
finaliseH Env{..} FinaliseReq{..} = do
  -- TODO WG: Realism - signature verification
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
              res <- liftIO (submit tx mockState)
              case res of
                Left reason ->
                  pure $ FinaliseResp txId now (Rejected reason)
                Right _ -> do
                  liftIO . atomically $
                    modifyTVar' pending (Map.delete wantedTxId)
                  pure $ FinaliseResp txId now Finalised
