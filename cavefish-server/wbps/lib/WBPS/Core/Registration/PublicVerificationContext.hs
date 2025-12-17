module WBPS.Core.Registration.PublicVerificationContext (
  PublicVerificationContext (..),
) where

import Data.Aeson (Value)
import GHC.Generics (Generic)
import Path (Abs, File, Path)

data PublicVerificationContext = PublicVerificationContext {filePath :: Path Abs File, asJson :: Value}
  deriving (Eq, Show, Ord, Generic)
