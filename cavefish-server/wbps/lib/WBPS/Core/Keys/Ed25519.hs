module WBPS.Core.Keys.Ed25519 (
  UserWalletPublicKey (..),
  UserCommitmentPublicKey (..),
  KeyPair (..),
  PublicKey (..),
  PrivateKey (..),
  publicKey,
  privateKey,
  userWalletPK,
  generateKeyPair,
  generateKeyTuple,
) where

import Cardano.Crypto.DSIGN.Ed25519 (Ed25519DSIGN)
import Control.Monad.IO.Class (MonadIO)
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
  deriving newtype (ToJSON, FromJSON, IsString, Ord, Show, Eq)

newtype UserCommitmentPublicKey = UserCommitmentPublicKey PublicKey
  deriving newtype (ToJSON, FromJSON, IsString, Ord, Show, Eq)

newtype KeyPair = KeyPair (Adapter.KeyPair Ed25519DSIGN)
  deriving newtype (Show, Eq, ToJSON, FromJSON)

newtype PublicKey = PublicKey (Adapter.PublicKey Ed25519DSIGN)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsString, Ord)

newtype PrivateKey = PrivateKey (Adapter.PrivateKey Ed25519DSIGN)
  deriving newtype (Show, Eq, ToJSON, FromJSON, IsString)

publicKey :: KeyPair -> PublicKey
publicKey (KeyPair (Adapter.KeyPair {..})) = PublicKey verificationKey

privateKey :: KeyPair -> PrivateKey
privateKey (KeyPair (Adapter.KeyPair {..})) = PrivateKey signatureKey

userWalletPK :: KeyPair -> UserWalletPublicKey
userWalletPK (KeyPair (Adapter.KeyPair {..})) = UserWalletPublicKey . PublicKey $ verificationKey

generateKeyPair :: forall m. MonadIO m => m KeyPair
generateKeyPair = KeyPair <$> Adapter.generateKeyPair @Ed25519DSIGN

generateKeyTuple :: forall m. MonadIO m => m (PrivateKey, PublicKey)
generateKeyTuple = do
  k <- generateKeyPair
  return (privateKey k, publicKey k)
