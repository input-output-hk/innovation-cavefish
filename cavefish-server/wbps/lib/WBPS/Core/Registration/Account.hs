-- | Module defining the AccountId type and AccountCreated event for user account registration.
module WBPS.Core.Registration.Account (
  AccountId (..),
  -- | Unique identifier for a user account
  AccountCreated (..),
  -- | Event representing the creation of a new account
  deriveId,
  -- | Function to derive AccountId from UserWalletPublicKey
) where

import WBPS.Core.Groth16.Setup (Setup)
import WBPS.Core.Keys.Ed25519 (PublicKey (PublicKey), UserWalletPublicKey (UserWalletPublicKey))

newtype AccountId = AccountId String deriving (Show, Eq)

deriveId :: UserWalletPublicKey -> AccountId
deriveId (UserWalletPublicKey (PublicKey x)) = AccountId . show $ x

data AccountCreated
  = AccountCreated
  { userWalletPublicKey :: UserWalletPublicKey
  , setup :: Setup
  }
  deriving (Ord, Eq, Show)
