module WBPS.Core.Keys.Ed25519 (
  UserWalletPublicKey (..),
  UserCommitmentPublicKey (..),
  KeyPair (..),
  PublicKey (..),
  PrivateKey (..),
  PaymentVerificationKey (..),
  PaymentAddess (..),
  getPublicKey,
  getPrivateKey,
  userWalletPK,
  generateKeyPair,
  generateKeyTuple,
  generateWallet,
  Wallet (..),
) where

import Cardano.Api qualified as Api
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

getPublicKey :: KeyPair -> PublicKey
getPublicKey (KeyPair (Adapter.KeyPair {..})) = PublicKey verificationKey

getPrivateKey :: KeyPair -> PrivateKey
getPrivateKey (KeyPair (Adapter.KeyPair {..})) = PrivateKey signatureKey

newtype PaymentVerificationKey = PaymentVerificationKey (Api.VerificationKey Api.PaymentKey)
  deriving newtype (Show, Eq, IsString)

newtype PaymentAddess = PaymentAddess {unPaymentAddess :: Text}
  deriving newtype (Show, Eq, IsString, ToJSON, FromJSON)

paymentAddress' :: PaymentVerificationKey -> PaymentAddess
paymentAddress' (PaymentVerificationKey paymentVKey) =
  PaymentAddess . Api.serialiseAddress $
    Api.makeShelleyAddressInEra
      Api.ShelleyBasedEraConway
      (Api.Testnet (Api.NetworkMagic 1))
      (Api.PaymentCredentialByKey (Api.verificationKeyHash paymentVKey))
      Api.NoStakeAddress

paymentVerificationKey' ::
  KeyPair ->
  PaymentVerificationKey
paymentVerificationKey' kp =
  let PublicKey adapterPk = getPublicKey kp
   in PaymentVerificationKey $
        either (error . show) id $
          Api.deserialiseFromRawBytes
            (Api.AsVerificationKey Api.AsPaymentKey)
            (Adapter.toByteString adapterPk)

userWalletPK :: KeyPair -> UserWalletPublicKey
userWalletPK (KeyPair (Adapter.KeyPair {..})) = UserWalletPublicKey . PublicKey $ verificationKey

generateKeyPair :: forall m. MonadIO m => m KeyPair
generateKeyPair = KeyPair <$> Adapter.generateKeyPair @Ed25519DSIGN

generateKeyTuple :: forall m. MonadIO m => m (PrivateKey, PublicKey)
generateKeyTuple = do
  k <- generateKeyPair
  return (getPrivateKey k, getPublicKey k)

data Wallet = Wallet
  { keyPair :: KeyPair
  , publicKey :: UserWalletPublicKey
  , paymentVerificationKey :: PaymentVerificationKey
  , paymentAddress :: PaymentAddess
  }

generateWallet :: forall m. MonadIO m => m Wallet
generateWallet = do
  keyPair <- generateKeyPair
  return
    Wallet
      { keyPair
      , publicKey = userWalletPK keyPair
      , paymentVerificationKey = paymentVerificationKey' keyPair
      , paymentAddress = paymentAddress' (paymentVerificationKey' keyPair)
      }
