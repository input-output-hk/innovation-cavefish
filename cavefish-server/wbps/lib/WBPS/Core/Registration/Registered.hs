-- | Module defining the AccountId type and Registered event for user account registration.
module WBPS.Core.Registration.Registered (
  AccountId (..),
  -- | Unique identifier for a user account
  Registered (..),
  -- | Event representing the creation of a new account
  deriveId,
  -- | Function to derive AccountId from UserWalletPublicKey
) where

import WBPS.Core.Registration.Artefacts.Groth16.Setup (Setup)
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (PublicKey (PublicKey), UserWalletPublicKey (UserWalletPublicKey))

newtype AccountId = AccountId String deriving (Show, Eq)

deriveId :: UserWalletPublicKey -> AccountId
deriveId (UserWalletPublicKey (PublicKey x)) = AccountId . show $ x

data Registered
  = Registered
  { userWalletPublicKey :: UserWalletPublicKey
  , setup :: Setup
  }
  deriving (Ord, Eq, Show)
