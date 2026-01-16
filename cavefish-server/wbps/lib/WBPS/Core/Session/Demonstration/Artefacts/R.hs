module WBPS.Core.Session.Demonstration.Artefacts.R (
  R (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.String (IsString)
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 qualified as Ed25519

newtype R = R Ed25519.PublicKey
  deriving newtype (Eq, Show, Ord, FromJSON, ToJSON, IsString)
