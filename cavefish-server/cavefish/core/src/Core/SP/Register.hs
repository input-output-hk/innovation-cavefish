{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Core.SP.Register (
  handle,
  Inputs (..),
  Outputs (..),
  mkSignerKey,
) where

import Cardano.Crypto.DSIGN (rawDeserialiseVerKeyDSIGN)
import Control.Concurrent.STM (atomically, modifyTVar')
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader (ask))
import Core.Api.AppContext (
  AppM,
  Env (
    Env,
    clientRegistration,
    spSk,
    wbpsScheme
  ),
 )
import Core.Api.State (
  ClientId (ClientId),
  ClientRegistration (ClientRegistration),
  parsePublicKey,
  renderPublicKey,
 )
import Core.Proof (parseHex, renderHex)
import Crypto.Error (CryptoFailable (CryptoFailed, CryptoPassed))
import Crypto.PubKey.Ed25519 qualified as Ed
import Data.Aeson (
  FromJSON (parseJSON),
  KeyValue ((.=)),
  ToJSON (toJSON),
  Value,
  object,
  withObject,
  (.:),
 )
import Data.ByteArray qualified as BA
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import GHC.Generics (Generic)
import Servant (
  err422,
  err500,
  errBody,
  throwError,
 )
import WBPS qualified
import WBPS.Adapter.CardanoCryptoClass.Crypto qualified as W
import WBPS.Core (SignerKey, WbpsPublicKey)

data Inputs = Inputs
  { userPublicKey :: Ed.PublicKey
  , xPublicKey :: WbpsPublicKey
  }
  deriving (Eq, Show, Generic)

mkSignerKey :: Ed.PublicKey -> Maybe SignerKey
mkSignerKey pk =
  W.PublicKey <$> rawDeserialiseVerKeyDSIGN (BA.convert pk)

handle :: Inputs -> AppM Outputs
handle Inputs {userPublicKey, xPublicKey} = do
  Env {clientRegistration, spSk, wbpsScheme} <- ask
  signerKey <-
    maybe
      (throwError err422 {errBody = "invalid signer public key"})
      pure
      (mkSignerKey userPublicKey)
  result <- liftIO $ WBPS.withFileSchemeIO wbpsScheme (WBPS.getVerificationContext signerKey)
  verificationValue <-
    either
      ( \failures -> throwError err500 {errBody = BL8.pack ("verification context unavailable: " <> show failures)}
      )
      pure
      result
  let userPublicKeyHex = renderHex (renderPublicKey userPublicKey)
  storeResult <-
    liftIO $
      WBPS.withFileSchemeIO
        wbpsScheme
        (WBPS.storeAccountArtifacts signerKey userPublicKeyHex xPublicKey)
  either
    ( \failures ->
        throwError err500 {errBody = BL8.pack ("failed to persist account artifacts: " <> show failures)}
    )
    pure
    storeResult
  uuid <- liftIO nextRandom
  liftIO . atomically $
    modifyTVar'
      clientRegistration
      (Map.insert (ClientId uuid) (ClientRegistration userPublicKey xPublicKey))
  pure Outputs {id = uuid, spPk = Ed.toPublic spSk, verificationContext = verificationValue}

instance FromJSON Inputs where
  parseJSON = withObject "Inputs" $ \o -> do
    userPublicKey <- parsePublicKey =<< o .: "userPublicKey"
    xPublicKey <- o .: "X"
    pure Inputs {userPublicKey, xPublicKey}

instance ToJSON Inputs where
  toJSON Inputs {..} =
    object
      [ "userPublicKey" .= renderHex (renderPublicKey userPublicKey)
      , "X" .= xPublicKey
      ]

data Outputs = Outputs
  { id :: UUID
  , spPk :: Ed.PublicKey
  , verificationContext :: Value
  }
  deriving (Eq, Show, Generic)

instance ToJSON Outputs where
  toJSON Outputs {id, spPk, verificationContext} =
    object
      [ "id" .= id
      , "spPk" .= renderHex (BA.convert spPk)
      , "verificationContext" .= verificationContext
      ]

instance FromJSON Outputs where
  parseJSON = withObject "Outputs" $ \o -> do
    id <- o .: "id"
    spHex :: Text <- o .: "spPk"
    bytes <- parseHex spHex
    verificationContext <- o .: "verificationContext"
    case Ed.publicKey bytes of
      CryptoFailed err -> fail ("invalid service public key: " <> show err)
      CryptoPassed pk -> pure Outputs {spPk = pk, id, verificationContext}
