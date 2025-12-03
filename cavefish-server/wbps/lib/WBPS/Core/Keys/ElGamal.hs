{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WBPS.Core.Keys.ElGamal (
  KeyPair (..),
  EncryptionKey (..),
  DecryptionKey (..),
  generateKeyPair,
) where

import Cardano.Crypto.DSIGN (Ed25519DSIGN, seedSizeDSIGN)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Error (CryptoFailable (..))
import Crypto.PubKey.Ed25519 qualified as E
import Crypto.Random (getRandomBytes)
import Data.Aeson (
  FromJSON (..),
  ToJSON (..),
  Value (..),
  object,
  withObject,
  withText,
  (.:),
  (.=),
 )
import Data.ByteArray as BA
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Text.Hex qualified as Hex

-- | Helper wrapper to derive hex-encoded JSON for ByteString.
newtype HexBytes = HexBytes ByteString
  deriving (Show, Eq, Ord)

instance ToJSON HexBytes where
  toJSON (HexBytes bs) = String (Hex.encodeHex bs)

instance FromJSON HexBytes where
  parseJSON = withText "HexBytes" $ \t ->
    case Hex.decodeHex t of
      Just bs -> pure (HexBytes bs)
      Nothing -> fail "Invalid hex-encoded bytes"

-- | Public encryption key (X = x·G)
newtype EncryptionKey = EncryptionKey ByteString
  deriving (Eq, Show)
  deriving (ToJSON, FromJSON, Ord) via HexBytes

-- | Secret decryption key (scalar seed x)
newtype DecryptionKey = DecryptionKey ByteString
  deriving (Eq, Show)
  deriving (ToJSON, FromJSON, Ord) via HexBytes

data KeyPair = KeyPair
  { ek :: EncryptionKey
  , dk :: DecryptionKey
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON KeyPair where
  toJSON (KeyPair ek' dk') =
    object
      [ "ek" .= ek'
      , "dk" .= dk'
      ]

instance FromJSON KeyPair where
  parseJSON = withObject "KeyPair" $ \o ->
    KeyPair
      <$> o .: "ek"
      <*> o .: "dk"

-- | Generate an ElGamal keypair (ek, dk) on Ed25519.
--
-- We:
--   * use Cardano's Ed25519DSIGN seed size (usually 32 bytes)
--   * draw that many random bytes
--   * feed them to Crypto.PubKey.Ed25519.secretKey
--   * derive the public key with toPublic
--   * store both as raw bytes, hex-encoded in JSON via HexBytes
generateKeyPair :: forall m. MonadIO m => m KeyPair
generateKeyPair = liftIO $ do
  -- How many bytes of entropy does Ed25519DSIGN expect?
  let n :: Int
      n = fromIntegral (seedSizeDSIGN (Proxy :: Proxy Ed25519DSIGN))

  -- Draw n random bytes as seed
  seed :: ByteString <- getRandomBytes n

  case E.secretKey seed of
    CryptoPassed sk ->
      let pk = E.toPublic sk
          skBytes = BA.convert sk :: ByteString
          pkBytes = BA.convert pk :: ByteString
       in pure
            KeyPair
              { ek = EncryptionKey pkBytes
              , dk = DecryptionKey skBytes
              }
    CryptoFailed err ->
      -- You might want to lift this into MonadError instead of failing in IO,
      -- but this is at least total and clear.
      fail ("ElGamal KeyGen failed: " ++ show err)
