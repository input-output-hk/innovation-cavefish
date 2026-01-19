module WBPS.Core.Registration.Artefacts.Groth16.Setup (
  Setup (..),
  PublicVerificationContextAsJSON (..),
  PublicVerificationContext (..),
) where

import Data.Aeson (Value)
import Data.Aeson.Types (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Path (Abs, File, Path)
import WBPS.Core.Registration.Artefacts.Keys.ElGamal qualified as ElGamal

newtype PublicVerificationContextAsJSON
  = PublicVerificationContextAsJSON {unPublicVerificationContextAsJSON :: Value}
  deriving stock (Eq, Show, Ord, Generic)
  deriving newtype (FromJSON, ToJSON)

data PublicVerificationContext
  = PublicVerificationContext
  { filePath :: Path Abs File
  , asJson :: PublicVerificationContextAsJSON
  }
  deriving (Eq, Show, Ord, Generic, FromJSON, ToJSON)

data Setup = Setup
  { provingKey :: Path Abs File
  , encryptionKeys :: ElGamal.KeyPair
  , publicVerificationContext :: PublicVerificationContext
  }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)
