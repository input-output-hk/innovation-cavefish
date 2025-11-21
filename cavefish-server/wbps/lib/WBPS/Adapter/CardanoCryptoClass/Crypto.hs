{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module WBPS.Adapter.CardanoCryptoClass.Crypto (
  KeyPair (..),
  PrivateKey (..),
  PublicKey (..),
  Hexadecimal,
  Codec (..),
  sign,
  generateKeyPair,
  Ed25519DSIGN,
  DSIGNAlgorithm (..),
  ToByteString (..),
  FromByteString (..),
  manualTesting,
) where

import Cardano.Crypto.DSIGN
import Cardano.Crypto.Hash (ByteString)
import Cardano.Crypto.Seed (mkSeedFromBytes)
import Crypto.Random
import Data.Aeson as A hiding (decode, decode', encode)
import Data.ByteString qualified as BS
import Data.Coerce (coerce)
import Data.Data (Proxy (..))
import Data.List qualified as T
import Data.String
import Data.Text as Text hiding (drop)
import GHC.Generics
import GHC.Stack (HasCallStack)
import Text.Hex (decodeHex, encodeHex)

-- | Generate a fresh DSIGN keypair (e.g. Ed25519DSIGN).
generateKeyPair ::
  forall a m.
  (DSIGNAlgorithm a, MonadRandom m) =>
  m (KeyPair a)
generateKeyPair = do
  -- Determine how many bytes of entropy are needed for this DSIGN algorithm
  let nBytes :: Int
      nBytes = fromIntegral (seedSizeDSIGN (Proxy @a))

  -- Draw random bytes inside MonadRandom
  seedBytes <- getRandomBytes nBytes

  -- Turn them into a Seed and derive signing/verifying keys
  let seed = mkSeedFromBytes seedBytes
      sk = genKeyDSIGN @a seed
      vk = deriveVerKeyDSIGN sk

  pure
    KeyPair
      { signatureKey = PrivateKey sk
      , verificationKey = PublicKey vk
      }

data KeyPair a = KeyPair
  { signatureKey :: PrivateKey a
  , verificationKey :: PublicKey a
  }
  deriving (Show, Eq)

newtype PrivateKey a = PrivateKey (SignKeyDSIGN a)

newtype PublicKey a = PublicKey (VerKeyDSIGN a)

class ToByteString a where
  toByteString :: a -> ByteString

class FromByteString a where
  fromByteString :: ByteString -> a

instance DSIGNAlgorithm a => FromByteString (PrivateKey a) where
  fromByteString =
    PrivateKey
      . ( \case
            Just rawPrivateKey -> rawPrivateKey
            Nothing -> error "Failed to decode signature key from Hexadecimal format"
        )
      . rawDeserialiseSignKeyDSIGN @a

instance DSIGNAlgorithm a => ToByteString (PrivateKey a) where
  toByteString (PrivateKey a) = rawSerialiseSignKeyDSIGN a

instance DSIGNAlgorithm a => ToByteString (PublicKey a) where
  toByteString (PublicKey a) = rawSerialiseVerKeyDSIGN a

instance ToByteString Hexadecimal where
  toByteString (Hexadecimal bs) = bs

instance FromByteString Hexadecimal where
  fromByteString = Hexadecimal

instance DSIGNAlgorithm a => FromJSON (PublicKey a) where
  parseJSON (A.String s) = pure . fromString @(PublicKey a) . removeOx . Text.unpack $ s
    where
      removeOx = T.drop 2
  parseJSON _ = fail "Expected a string"

instance DSIGNAlgorithm a => FromJSON (PrivateKey a) where
  parseJSON (A.String s) = pure . fromString @(PrivateKey a) . removeOx . Text.unpack $ s
    where
      removeOx = T.drop 2
  parseJSON _ = fail "Expected a string"

instance DSIGNAlgorithm a => FromJSON (KeyPair a) where
  parseJSON (Object v) =
    KeyPair
      <$> v .: "secretSeed"
      <*> v .: "publicKey"
  parseJSON _ = fail "Expected an object"

sign ::
  forall v.
  (ContextDSIGN v ~ (), HasCallStack, Signable v ByteString, DSIGNAlgorithm v) =>
  PrivateKey v ->
  ByteString ->
  ByteString
sign (PrivateKey privateKey) message =
  rawSerialiseSigDSIGN . signDSIGN () message $ privateKey

class Codec a where
  encode :: a -> Text.Text
  decode :: Text.Text -> Maybe a

  decode' :: Text.Text -> a
  decode' =
    ( \case
        Just a -> a
        Nothing -> error "Failed to decode"
    )
      . decode

newtype Hexadecimal = Hexadecimal ByteString

instance Codec Hexadecimal where
  encode = encodeHex . coerce
  decode = (Hexadecimal <$>) . decodeHex

instance DSIGNAlgorithm a => IsString (PrivateKey a) where
  fromString =
    fromByteString @(PrivateKey a)
      . toByteString @Hexadecimal
      . decode' @Hexadecimal
      . Text.pack

instance DSIGNAlgorithm a => Show (PrivateKey a) where
  show =
    Text.unpack
      . encode @Hexadecimal
      . fromByteString @Hexadecimal
      . toByteString @(PrivateKey a)

instance DSIGNAlgorithm a => Show (PublicKey a) where
  show =
    Text.unpack
      . encode @Hexadecimal
      . fromByteString @Hexadecimal
      . toByteString @(PublicKey a)

instance DSIGNAlgorithm a => Eq (PrivateKey a) where
  (==) a b = show a == show b

instance DSIGNAlgorithm a => Eq (PublicKey a) where
  (==) a b = show a == show b

instance DSIGNAlgorithm a => Ord (PublicKey a) where
  (<=) a b = show a <= show b

instance DSIGNAlgorithm a => IsString (PublicKey a) where
  fromString =
    ( \case
        Just publicKey -> PublicKey publicKey
        Nothing -> error "Failed to decode signature key from Hexadecimal format"
    )
      . rawDeserialiseVerKeyDSIGN @a
      . ( \case
            Just seed -> seed
            Nothing -> error "Failed to decode seed in an Hexadecimal format"
        )
      . decodeHex
      . Text.pack

-- docker run -it --pull=always docker.io/parity/subkey:latest sign --hex --scheme Ed25519 --message 68656c6c6f2c20776f726c64 --suri 0x703a8f691df8a74a415bc1297f9d5fbd595cdc42c0defdca636a1beb7f24852d
-- docker run -it --pull=always docker.io/parity/subkey:latest generate --scheme Ed25519 --output-type json >  keys.json
manualTesting :: IO ()
manualTesting = do
  let message = toByteString . decode' @Hexadecimal $ "68656c6c6f2c20776f726c64"
      pair =
        KeyPair
          { signatureKey =
              fromString @(PrivateKey Ed25519DSIGN)
                "703a8f691df8a74a415bc1297f9d5fbd595cdc42c0defdca636a1beb7f24852d"
          , verificationKey =
              fromString @(PublicKey Ed25519DSIGN)
                "3a25f5e2af122ccbb607251303cc61e48caaef42a4fd2f11ddfdc9435871d806"
          }

      signedMessage = sign @Ed25519DSIGN (signatureKey pair) message

  putStrLn $
    "signedMessage         : " <> (Text.unpack . encode . fromByteString @Hexadecimal $ signedMessage)
  putStrLn
    "expected signedMessage:0x9adfcd820533d73e7cf6b660e6511d6342202f0bd2ee1ecd7e5348431487fb5c82710080c405f9466f5885f43ffcc26e12378e938bfd6a3d14d7794d28d16306"
  putStrLn $ "pubkey: " <> (show . verificationKey $ pair)
  putStrLn "expected signedMessage:3a25f5e2af122ccbb607251303cc61e48caaef42a4fd2f11ddfdc9435871d806"
