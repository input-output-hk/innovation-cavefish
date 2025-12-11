{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Core.SP.Register (
  handle,
  Inputs (..),
  Outputs (..),
) where

import Control.Monad.Reader (MonadReader (ask))
import Core.Api.ServerContext (
  ServerContext (ServerContext, wbpsServices),
  ServerM,
  WBPSServices (..),
 )
import Data.Aeson (FromJSON, ToJSON, Value)
import GHC.Generics (Generic)
import WBPS.Core.Keys.Ed25519 (
  UserWalletPublicKey,
 )
import WBPS.Core.Keys.ElGamal (EncryptionKey)
import WBPS.Core.Keys.ElGamal qualified as ElGamal
import WBPS.Registration (
  AccountCreated (..),
  PublicVerificationContext (..),
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

handle :: Inputs -> ServerM Outputs
handle Inputs {userWalletPublicKey} = do
  ServerContext {wbpsServices = WBPSServices {register}} <- ask
  AccountCreated
    { publicVerificationContext = PublicVerificationContext {asJson = publicVerificationContext}
    , encryptionKeys = ElGamal.KeyPair {..}
    } <-
    register userWalletPublicKey
  pure Outputs {..}
