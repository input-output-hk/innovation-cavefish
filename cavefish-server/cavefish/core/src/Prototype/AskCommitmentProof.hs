{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Prototype.AskCommitmentProof (handle, Inputs (..), Outputs (..)) where

import Cavefish (
  CavefishServerM,
 )
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import WBPS.Core.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Keys.Ed25519 qualified as Ed25519

data Inputs = Inputs
  { userWalletPublicKey :: UserWalletPublicKey
  , bigR :: Ed25519.PublicKey
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data Outputs = Outputs
  { challenge :: Int
  , proof :: Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

handle :: Inputs -> CavefishServerM Outputs
handle Inputs {userWalletPublicKey = _, bigR = _} =
  pure Outputs {proof = "proof", challenge = 0}

-- CavefishServices {..} <- ask
-- now <- liftIO getCurrentTime
-- keypair <- Ed25519.generateKeyPair -- N.H to fix
-- (_, dk) <- liftIO PKE.generateKeyPair
-- case parseTxIdHex txId of
--   Nothing ->
--     throwError err400 {errBody = "malformed tx id"}
--   Just wantedTxId -> do
--     mp <- lookupPendingEntry pending wantedTxId
--     case mp of
--       Nothing ->
--         throwError err404 {errBody = "unknown or expired tx"}
--       Just pendingEntry@Pending {expiry, commitment, ciphertext, txAbsHash, tx} -> do
--         when (now > expiry) $ do
--           removePendingEntry pending wantedTxId
--           throwError err410 {errBody = "pending expired"}
--         _ <- either throwError pure (decryptPendingPayload dk pendingEntry)
--         -- mClient <- lookupClientRegistration clientRegistration creator
--         let mClient = Nothing
--         case mClient of
--           Nothing ->
--             throwError err403 {errBody = "unknown client"}
--           Just _ ->
--             case commitment of
--               Just _ ->
--                 throwError err409 {errBody = "commitment already recorded"}
--               Nothing -> do
--                 liftIO . atomically $
--                   modifyTVar' pending (Map.adjust (\p -> p {commitment = Just bigR}) wantedTxId)
--                 let txIdVal = Api.getTxId (Api.getTxBody tx)
--                     commitmentBytes = ciphertextDigest ciphertext
--                     proof = ProofEd25519 (mkProof (Ed25519.privateKey keypair) txIdVal txAbsHash commitmentBytes)
--                 pure Outputs {proof = proof, challenge = 0}
