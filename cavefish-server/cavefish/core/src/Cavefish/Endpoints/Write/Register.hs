{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Cavefish.Endpoints.Write.Register (
  handle,
  Inputs (..),
  Outputs (..),
) where

import Cavefish (
  CavefishServerM,
  CavefishServices (CavefishServices, wbpsService),
 )
import Cavefish.Services.WBPS qualified as Service
import Control.Monad.Reader (MonadReader (ask))
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

handle :: Inputs -> CavefishServerM Outputs
handle Inputs {userWalletPublicKey} = do
  CavefishServices {wbpsService = Service.WBPS {register}} <- ask
  AccountCreated
    { publicVerificationContext = PublicVerificationContext {asJson = publicVerificationContext}
    , encryptionKeys = ElGamal.KeyPair {..}
    } <-
    register userWalletPublicKey
  pure Outputs {..}
