{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Core.SP.AskSubmission (handle, Inputs (..), Outputs (..), FinaliseResult (..)) where

import Control.Concurrent.STM (atomically, modifyTVar')
import Control.Monad (unless)
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
import Core.Api.Messages (
  decryptPendingPayload,
  lookupClientRegistration,
  lookupPendingEntry,
  parseTxIdHex,
  removePendingEntry,
  verifyClientSignature,
 )
import Core.Api.State (
  ClientRegistration (ClientRegistration, userPublicKey),
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
 )
import Core.Proof (parseHex, renderHex)
import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toJSON),
  object,
  withObject,
  (.:),
  (.=),
 )
import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Servant (
  err410,
  errBody,
  throwError,
 )

handle :: Inputs -> AppM Outputs
handle Inputs {..} = do
  env@Env {..} <- ask
  now <- liftIO getCurrentTime
  case parseTxIdHex txId of
    Nothing ->
      pure $ Outputs txId now (Rejected "malformed tx id")
    Just wantedTxId -> do
      mp <- lookupPendingEntry pending wantedTxId
      case mp of
        Nothing ->
          pure $ Outputs txId now (Rejected "unknown or expired tx")
        Just
          pendingEntry@Pending {commitment, expiry, creator, ciphertext, auxNonce, rho, tx, txAbsHash, mockState}
            | now > expiry -> do
                removePendingEntry pending wantedTxId
                pure $ Outputs txId now (Rejected "pending expired")
            | otherwise -> do
                unless
                  (isJust commitment)
                  (throwError err410 {errBody = "commitment must be made before submission"})
                _payload <- either throwError pure (decryptPendingPayload env pendingEntry)
                mClient <- lookupClientRegistration clientRegistration creator
                case mClient of
                  Nothing ->
                    pure $ Outputs txId now (Rejected "unknown client")
                  Just ClientRegistration {userPublicKey} ->
                    if verifyClientSignature userPublicKey txAbsHash lcSig
                      then do
                        res <- liftIO (submit tx mockState)
                        case res of
                          Left reason ->
                            pure $ Outputs txId now (Rejected reason)
                          Right _ -> do
                            let completed = Completed {tx, submittedAt = now, creator}
                            liftIO . atomically $ do
                              modifyTVar' pending (Map.delete wantedTxId)
                              modifyTVar' complete (Map.insert wantedTxId completed)
                            pure $ Outputs txId now Finalised
                      else pure $ Outputs txId now (Rejected "invalid client signature")

data Inputs = Inputs
  { txId :: Text
  , lcSig :: ByteString
  }
  deriving (Eq, Show, Generic)

instance FromJSON Inputs where
  parseJSON = withObject "Inputs" $ \o -> do
    txId <- o .: "txId"
    sigHex :: Text <- o .: "lcSig"
    lcSig <- parseHex sigHex
    pure Inputs {..}

instance ToJSON Inputs where
  toJSON Inputs {..} =
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

data Outputs = Outputs
  { txId :: Text
  , submittedAt :: UTCTime
  , result :: FinaliseResult
  }
  deriving (Eq, Show, Generic)

instance ToJSON Outputs

instance FromJSON Outputs
