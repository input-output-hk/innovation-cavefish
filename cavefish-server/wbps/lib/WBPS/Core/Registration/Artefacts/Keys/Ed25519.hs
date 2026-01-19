module WBPS.Core.Registration.Artefacts.Keys.Ed25519 (
  UserWalletPublicKey (..),
  KeyPair (..),
  PublicKey (..),
  PrivateKey (..),
  PaymentVerificationKey (..),
  PaymentAddess (..),
  getPublicKey,
  getPrivateKey,
  userWalletPK,
  userWalletPublicKeyToWord8s,
  generateKeyPair,
  generateKeyTuple,
  generateWallet,
  Wallet (..),
) where

import Cardano.Api qualified as Api
import Cardano.Api qualified as C
import Cardano.Crypto.DSIGN.Ed25519 (Ed25519DSIGN)
import Control.Monad.IO.Class (MonadIO)
import Cooked (IsTxSkelOutAllowedOwner (toPKHOrVScript), User (UserPubKey))
import Data.Aeson (FromJSON, ToJSON)
import Data.Coerce (coerce)
import Data.String (IsString)
import Data.Text (Text)
import Data.Word (Word8)
import Ledger.Address qualified as Ledger
import PlutusLedgerApi.V3 qualified as Api
import WBPS.Adapter.CardanoCryptoClass.Crypto qualified as Adapter
import WBPS.Adapter.Data.ByteString (bytesToBitsLE)

newtype UserWalletPublicKey = UserWalletPublicKey PublicKey
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
  deriving newtype (Show, Eq)

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

userWalletPublicKeyToWord8s :: UserWalletPublicKey -> [Word8]
userWalletPublicKeyToWord8s (UserWalletPublicKey (PublicKey pk)) =
  bytesToBitsLE (Adapter.toByteString pk)

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
  deriving (Show)

instance IsTxSkelOutAllowedOwner Wallet where
  toPKHOrVScript = UserPubKey . walletPubKeyHash

walletPubKeyHash :: Wallet -> Api.PubKeyHash
walletPubKeyHash Wallet {paymentAddress = PaymentAddess addrText} =
  maybe (error "Invalid wallet address in initial distribution") coerce $
    C.deserialiseAddress (C.AsAddressInEra C.AsConwayEra) addrText
      >>= Ledger.cardanoPubKeyHash

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
