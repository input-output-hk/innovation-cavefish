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
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader (ask))
import Core.Api.AppContext (AppM, Env (Env, wbpsScheme))
import Data.Aeson (
  FromJSON (parseJSON),
  KeyValue ((.=)),
  ToJSON (toJSON),
  object,
  withObject,
  (.:),
 )
import Data.ByteString.Lazy.Char8 qualified as BL8
import GHC.Generics (Generic)
import Servant (
  err422,
  err500,
  errBody,
  throwError,
 )
import WBPS (
  AccountCreated (
    AccountCreated,
    encryptionKeys,
    provingKey,
    publicVerificationContext,
    userWalletPublicKey
  ),
  PublicVerificationContext (PublicVerificationContext, asJson),
  RegistrationFailed (AccountAlreadyRegistered),
  register,
  withFileSchemeIO,
 )
import WBPS.Core.Keys.Ed25519 (
  UserWalletPublicKey,
 )
import WBPS.Core.Keys.ElGamal (EncryptionKey)
import WBPS.Core.Keys.ElGamal qualified as ElGamal

newtype Inputs = Inputs
  { userWalletPublicKey :: UserWalletPublicKey
  }
  deriving (Eq, Show, Generic)

data Outputs = Outputs
  { ek :: EncryptionKey
  , publicVerificationContext :: PublicVerificationContext
  }
  deriving (Eq, Show, Generic)

handle :: Inputs -> AppM Outputs
handle Inputs {userWalletPublicKey} = do
  Env {wbpsScheme} <- ask
  liftIO (WBPS.withFileSchemeIO wbpsScheme (WBPS.register userWalletPublicKey))
    >>= \case
      (Left [AccountAlreadyRegistered _]) -> throwError err422 {errBody = BL8.pack "Account Already Registered"}
      (Left e) -> throwError err500 {errBody = BL8.pack ("Unexpected event" ++ show e)}
      (Right AccountCreated {encryptionKeys = ElGamal.KeyPair {..}, ..}) -> pure Outputs {..}

instance FromJSON Inputs where
  parseJSON = withObject "Inputs" $ \o -> do
    userWalletPublicKey <- o .: "userPublicKey"
    pure Inputs {userWalletPublicKey}

instance ToJSON Outputs where
  toJSON Outputs {ek, publicVerificationContext = PublicVerificationContext {asJson}} =
    object
      [ "ek" .= toJSON ek
      , "verificationContext" .= asJson
      ]
