module WBPS.Core.Keys.Ed25519 (
  UserWalletPublicKey (..),
  UserCommitmentPublicKey (..),
  KeyPair (..),
  PublicKey (..),
  publicKey,
  userWalletPK,
  generateKeyPair,
) where

import Cardano.Crypto.DSIGN.Ed25519 (Ed25519DSIGN)
import Crypto.Random (MonadRandom)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.=))
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as B16
import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)
import WBPS.Adapter.CardanoCryptoClass.Crypto qualified as Adapter

newtype UserWalletPublicKey = UserWalletPublicKey PublicKey
  deriving (Show, Eq)
  deriving (FromJSON, IsString, Ord) via PublicKey

newtype UserCommitmentPublicKey = UserCommitmentPublicKey PublicKey
  deriving (Show, Eq)
  deriving (FromJSON, IsString, Ord) via PublicKey

newtype KeyPair = KeyPair (Adapter.KeyPair Ed25519DSIGN)
  deriving (Show, Eq)
  deriving (FromJSON) via (Adapter.KeyPair Ed25519DSIGN)

newtype PublicKey = PublicKey (Adapter.PublicKey Ed25519DSIGN)
  deriving (Show, Eq)
  deriving (FromJSON, IsString, Ord) via (Adapter.PublicKey Ed25519DSIGN)

publicKey :: KeyPair -> PublicKey
publicKey (KeyPair (Adapter.KeyPair {..})) = PublicKey verificationKey

userWalletPK :: KeyPair -> UserWalletPublicKey
userWalletPK (KeyPair (Adapter.KeyPair {..})) = UserWalletPublicKey . PublicKey $ verificationKey

generateKeyPair :: forall m. MonadRandom m => m KeyPair
generateKeyPair = KeyPair <$> Adapter.generateKeyPair @Ed25519DSIGN
