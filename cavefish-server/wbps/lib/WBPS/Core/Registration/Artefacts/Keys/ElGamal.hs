{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WBPS.Core.Registration.Artefacts.Keys.ElGamal (
  KeyPair (..),
  EncryptionKey (..),
  DecryptionKey (..),
  generateKeyPair,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toJSON),
  eitherDecode,
 )
import Data.ByteString.Lazy.Char8 qualified as BL8
import GHC.Generics (Generic)
import System.Process (readProcess)
import WBPS.Adapter.Math.AffinePoint (AffinePoint, parseIntegerValue)
import WBPS.Adapter.Math.Integer qualified as Integer

generateKeyPair :: forall m. MonadIO m => m KeyPair
generateKeyPair = liftIO $ do
  output <- readProcess "babyjubjub-keygen" [] ""
  case eitherDecode (BL8.pack output) of
    Left err -> fail ("babyjubjub-keygen: failed to decode JSON: " <> err)
    Right kp -> pure kp

data KeyPair = KeyPair
  { ek :: EncryptionKey
  , dk :: DecryptionKey
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Public encryption key (X = x·G)
newtype EncryptionKey = EncryptionKey {unEncryptionKey :: AffinePoint}
  deriving (Eq, Show, Generic, Ord)

-- | Secret decryption key (scalar seed x)
newtype DecryptionKey = DecryptionKey Integer
  deriving (Eq, Show, Ord)

instance ToJSON EncryptionKey where
  toJSON (EncryptionKey p) = toJSON p

instance FromJSON EncryptionKey where
  parseJSON v = EncryptionKey <$> parseJSON v

instance ToJSON DecryptionKey where
  toJSON (DecryptionKey n) = Integer.toValue n

instance FromJSON DecryptionKey where
  parseJSON = fmap DecryptionKey . WBPS.Adapter.Math.AffinePoint.parseIntegerValue
