{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cavefish.Endpoints.Write.Register (
  handle,
  Inputs (..),
  Outputs (..),
) where

import Cavefish (
  CavefishServerM,
  CavefishServices (CavefishServices, wbpsService),
 )
import Cavefish.Services.WBPS qualified as Service (WBPS (WBPS, register))
import Control.Monad.Reader (MonadReader (ask))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import WBPS.Core.Registration.Artefacts.Groth16.Setup (
  PublicVerificationContext (PublicVerificationContext, asJson),
  PublicVerificationContextAsJSON,
  Setup (Setup, encryptionKeys, publicVerificationContext),
 )
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Registration.Artefacts.Keys.ElGamal (EncryptionKey)
import WBPS.Core.Registration.Artefacts.Keys.ElGamal qualified as ElGamal (KeyPair (KeyPair, ek))
import WBPS.Core.Registration.Registered (Registered (Registered, setup))

newtype Inputs = Inputs
  { userWalletPublicKey :: UserWalletPublicKey
  }
  deriving newtype (Eq, Show, ToJSON, FromJSON)

data Outputs = Outputs
  { ek :: EncryptionKey
  , publicVerificationContext :: PublicVerificationContextAsJSON
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

handle :: Inputs -> CavefishServerM Outputs
handle Inputs {userWalletPublicKey} = do
  CavefishServices {wbpsService = Service.WBPS {register}} <- ask
  toOutputs <$> register userWalletPublicKey

toOutputs :: Registered -> Outputs
toOutputs
  Registered
    { setup =
      Setup
        { publicVerificationContext = PublicVerificationContext {asJson = publicVerificationContext}
        , encryptionKeys = ElGamal.KeyPair {ek}
        }
    } = Outputs {..}
