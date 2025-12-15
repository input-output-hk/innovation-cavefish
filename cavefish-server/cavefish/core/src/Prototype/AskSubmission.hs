{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Prototype.AskSubmission (handle, Inputs (..), Outputs (..), FinaliseResult (..)) where

import Cavefish (
  CavefishServerM,
 )
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toJSON),
  object,
  withObject,
  (.:),
  (.=),
 )
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time.Clock (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Prototype.Proof (parseHex, renderHex)

handle :: Inputs -> CavefishServerM Outputs
handle Inputs {} = do
  now <- liftIO getCurrentTime
  return $ Outputs "" now (Rejected "invalid client signature")

-- env@CavefishServices {..} <- ask
-- (ek, dk) <- liftIO ElGamal.generateKeyPair -- N.H to fix
-- case parseTxIdHex txId of
--   Nothing ->
--     pure $ Outputs txId now (Rejected "malformed tx id")
--   Just wantedTxId -> do
--     mp <- lookupPendingEntry pending wantedTxId
--     case mp of
--       Nothing ->
--         pure $ Outputs txId now (Rejected "unknown or expired tx")
--       Just
--         pendingEntry@Pending {commitment, expiry, creator, ciphertext, auxNonce, rho, tx, txAbsHash, mockState}
--           | now > expiry -> do
--               removePendingEntry pending wantedTxId
--               pure $ Outputs txId now (Rejected "pending expired")
--           | otherwise -> do
--               unless
--                 (isJust commitment)
--                 (throwError err410 {errBody = "commitment must be made before submission"})
--               _payload <- either throwError pure (decryptPendingPayload dk pendingEntry)
--               liftIO (WBPS.withFileSchemeIO wbpsScheme (WBPS.loadAccount creator))
--                 >>= \case
--                   (Left e) -> throwError err500 {errBody = BL8.pack ("Unexpected event" ++ show e)}
--                   (Right mClient) ->
--                     case mClient of
--                       Nothing ->
--                         pure $ Outputs txId now (Rejected "unknown client")
--                       Just AccountCreated {userWalletPublicKey} ->
--                         if verifyClientSignature userWalletPublicKey txAbsHash lcSig
--                           then do
--                             res <- liftIO (submit tx mockState)
--                             case res of
--                               Left reason ->
--                                 pure $ Outputs txId now (Rejected reason)
--                               Right _ -> do
--                                 let completed = Completed {tx, submittedAt = now, creator}
--                                 liftIO . atomically $ do
--                                   modifyTVar' pending (Map.delete wantedTxId)
--                                   modifyTVar' complete (Map.insert wantedTxId completed)
--                                 pure $ Outputs txId now Finalised
--                           else pure $ Outputs txId now (Rejected "invalid client signature")

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
