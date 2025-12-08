{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Core.SP.Register (
  handle,
  Inputs (..),
  Outputs (..),
) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader (ask))
import Core.Api.AppContext (AppM, Env (Env, wbpsScheme))
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.ByteString.Lazy.Char8 qualified as BL8
import GHC.Generics (Generic)
import Servant (
  err422,
  err500,
  errBody,
  throwError,
 )
import WBPS.Core.Keys.Ed25519 (
  UserWalletPublicKey,
 )
import WBPS.Core.Keys.ElGamal (EncryptionKey)
import WBPS.Core.Keys.ElGamal qualified as ElGamal
import WBPS.Registration (
  AccountCreated (
    AccountCreated,
    encryptionKeys,
    publicVerificationContext
  ),
  PublicVerificationContext (PublicVerificationContext, asJson),
  RegistrationFailed (AccountAlreadyRegistered),
  register,
  withFileSchemeIO,
 )

newtype Inputs = Inputs
  { userWalletPublicKey :: UserWalletPublicKey
  }
  deriving newtype (Eq, Show, ToJSON, FromJSON)

data Outputs = Outputs
  { ek :: EncryptionKey
  , publicVerificationContext :: Value
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

handle :: Inputs -> AppM Outputs
handle Inputs {userWalletPublicKey} = do
  Env {wbpsScheme} <- ask
  liftIO (withFileSchemeIO wbpsScheme (register userWalletPublicKey))
    >>= \case
      (Left [AccountAlreadyRegistered _]) -> throwError err422 {errBody = BL8.pack "Account Already Registered"}
      (Left e) -> throwError err500 {errBody = BL8.pack ("Unexpected event" ++ show e)}
      ( Right
          AccountCreated
            { publicVerificationContext = PublicVerificationContext {asJson = publicVerificationContext}
            , encryptionKeys = ElGamal.KeyPair {..}
            }
        ) ->
          pure Outputs {..}
