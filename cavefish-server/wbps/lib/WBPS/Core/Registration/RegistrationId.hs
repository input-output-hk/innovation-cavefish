module WBPS.Core.Registration.RegistrationId (RegistrationId (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.String (IsString)
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (PublicKey (PublicKey), UserWalletPublicKey (UserWalletPublicKey))

newtype RegistrationId
  = RegistrationId {userWalletPublicKey :: UserWalletPublicKey}
  deriving newtype (ToJSON, FromJSON, IsString, Ord, Eq)

instance Show RegistrationId where
  show (RegistrationId (UserWalletPublicKey (PublicKey x))) = show x
