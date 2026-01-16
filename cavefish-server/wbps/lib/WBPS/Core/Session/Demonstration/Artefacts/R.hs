module WBPS.Core.Session.Demonstration.Artefacts.R (
  R (..),
  toWord8s,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.String (IsString)
import Data.Word (Word8)
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 qualified as Ed25519

newtype R = R Ed25519.PublicKey
  deriving newtype (Eq, Show, Ord, FromJSON, ToJSON, IsString)

toWord8s :: R -> [Word8]
toWord8s (R rPk) =
  Ed25519.userWalletPublicKeyToWord8s (Ed25519.UserWalletPublicKey rPk)
