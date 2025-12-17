module WBPS.Core.Registration.Account (
  AccountId (..),
  AccountCreated (..),
  accountId,
) where

import Path (Abs, File, Path)
import WBPS.Core.Keys.Ed25519 (PublicKey (PublicKey), UserWalletPublicKey (UserWalletPublicKey))
import WBPS.Core.Keys.ElGamal qualified as ElGamal
import WBPS.Core.Registration.PublicVerificationContext (PublicVerificationContext)

newtype AccountId = AccountId String deriving (Show, Eq)

accountId :: UserWalletPublicKey -> AccountId
accountId (UserWalletPublicKey (PublicKey x)) = AccountId . show $ x

data AccountCreated
  = AccountCreated
  { userWalletPublicKey :: UserWalletPublicKey
  , provingKey :: Path Abs File
  , encryptionKeys :: ElGamal.KeyPair
  , publicVerificationContext :: PublicVerificationContext
  }
  deriving (Ord, Eq, Show)
