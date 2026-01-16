module WBPS.Core.Registration.Artefacts.Groth16.Setup (
  Setup (..),
  PublicVerificationContext (..),
) where

import Data.Aeson (Value)
import Data.Aeson.Types (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Path (Abs, File, Path)
import WBPS.Core.Registration.Artefacts.Keys.ElGamal qualified as ElGamal

data PublicVerificationContext = PublicVerificationContext {filePath :: Path Abs File, asJson :: Value}
  deriving (Eq, Show, Ord, Generic, FromJSON, ToJSON)

data Setup = Setup
  { provingKey :: Path Abs File
  , encryptionKeys :: ElGamal.KeyPair
  , publicVerificationContext :: PublicVerificationContext
  }
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)
