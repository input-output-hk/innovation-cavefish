{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Sp.Server (
  CavefishApi,
  server,
  Env (..),
) where

import Cardano.Api qualified as Api
import Control.Concurrent.STM
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Core.Intent (Intent, IntentW, satisfies, toInternalIntent)
import Core.Proof (
  Proof,
  mkProof,
  parseHex,
  renderHex,
 )
import Core.TxAbs (TxAbs)
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
import GHC.Generics (Generic)
import Servant
import Sp.State (Pending (..), PendingStore)

type CavefishApi =
  "prepare" :> ReqBody '[JSON] PrepareReq :> Post '[JSON] PrepareResp
    :<|> "finalise" :> ReqBody '[JSON] FinaliseReq :> Post '[JSON] FinaliseResp

newtype PrepareReq = PrepareReq
  { prIntent :: IntentW
  }
  deriving (Generic)
instance FromJSON PrepareReq
instance ToJSON PrepareReq

data PrepareResp = PrepareResp
  { txId :: Text
  , txAbs :: TxAbs Api.ConwayEra
  , proof :: Proof
  }
  deriving (Generic)
instance FromJSON PrepareResp
instance ToJSON PrepareResp

data FinaliseReq = FinaliseReq
  { frTxId :: Text
  , frLcSig :: ByteString
  }
  deriving (Generic)

instance FromJSON FinaliseReq where
  parseJSON = withObject "FinaliseReq" $ \o -> do
    frTxId <- o .: "frTxId"
    sigHex :: Text <- o .: "frLcSig"
    frLcSig <- parseHex sigHex
    pure FinaliseReq{..}
instance ToJSON FinaliseReq where
  toJSON FinaliseReq{..} =
    object
      [ "frTxId" .= frTxId
      , "frLcSig" .= renderHex frLcSig
      ]

data FinaliseResult
  = Finalised
  | Rejected Text
  deriving (Eq, Show, Generic)
instance ToJSON FinaliseResult
instance FromJSON FinaliseResult

data FinaliseResp = FinaliseResp
  { frTxId :: Text
  , frSubmittedAt :: UTCTime
  , frResult :: FinaliseResult
  }
  deriving (Eq, Show, Generic)
instance ToJSON FinaliseResp
instance FromJSON FinaliseResp

data Env = Env
  { envSpSk :: Ed.SecretKey
  , envPending :: PendingStore
  , envTtl :: NominalDiffTime
  , envBuild ::
      Intent Api.ConwayEra ->
      IO (Api.Tx Api.ConwayEra, TxAbs Api.ConwayEra)
  , envSubmit ::
      Api.Tx Api.ConwayEra ->
      IO (Either Text ())
  }

server :: Env -> Server CavefishApi
server env = prepareH env :<|> finaliseH env

prepareH :: Env -> PrepareReq -> Handler PrepareResp
prepareH Env{..} PrepareReq{..} = do
  intent <- liftIO $ either (ioError . userError . T.unpack) pure (toInternalIntent prIntent)
  (tx, txAbs) <- liftIO $ envBuild intent

  unless (satisfies intent txAbs) $
    throwError err422{errBody = "transaction does not satisfy intent"}

  let txBody = Api.getTxBody tx
      txId = Api.getTxId txBody
      txIdTxt = Api.serialiseToRawBytesHexText txId
      txAbsHash :: ByteString
      txAbsHash = hashTxAbs txAbs

      proof = mkProof envSpSk txId txAbsHash -- Proof stuff is very placeholdery
  now <- liftIO getCurrentTime
  let expiry = addUTCTime envTtl now
  liftIO . atomically $
    modifyTVar' envPending (Map.insert txId (Pending tx txAbsHash expiry))

  pure PrepareResp{txId = txIdTxt, txAbs, proof}
 where
  hashTxAbs :: (ToJSON a) => a -> ByteString
  hashTxAbs = BA.convert . (hashlazy @SHA256) . encode

finaliseH :: Env -> FinaliseReq -> Handler FinaliseResp
finaliseH Env{..} FinaliseReq{..} = do
  now <- liftIO getCurrentTime
  case Api.deserialiseFromRawBytesHex @Api.TxId (TE.encodeUtf8 frTxId) of
    Left _ ->
      pure $ FinaliseResp frTxId now (Rejected "malformed tx id")
    Right wantedTxId -> do
      mp <- liftIO . atomically $ do
        m <- readTVar envPending
        pure (Map.lookup wantedTxId m)
      case mp of
        Nothing ->
          pure $ FinaliseResp frTxId now (Rejected "unknown or expired tx")
        Just Pending{..} -> do
          when (now > pExpiry) $ do
            liftIO . atomically $
              modifyTVar' envPending (Map.delete wantedTxId)
            pure ()
          if now > pExpiry
            then pure $ FinaliseResp frTxId now (Rejected "pending expired")
            else do
              res <- liftIO (envSubmit pTx)
              case res of
                Left reason ->
                  pure $ FinaliseResp frTxId now (Rejected reason)
                Right _ -> do
                  liftIO . atomically $
                    modifyTVar' envPending (Map.delete wantedTxId)
                  pure $ FinaliseResp frTxId now Finalised
