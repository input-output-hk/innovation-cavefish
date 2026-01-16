{-# LANGUAGE OverloadedStrings #-}

module WBPS.Core.Session.Demonstration.Artefacts.Commitment (
  mkCommitment,
  commitmentPayloadToText,
  Commitment (..),
  CommitmentPayload (..),
  MessageLimbs (..),
  CommitmentId (..),
) where

import Cardano.Crypto.Hash (ByteString)
import Crypto.Hash (SHA256, hash)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), withArray)
import Data.Aeson.Types (Parser)
import Data.ByteArray qualified as BA
import Data.ByteString qualified as BS
import Data.String (IsString)
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Word (Word8)
import GHC.Generics (Generic)
import WBPS.Adapter.CardanoCryptoClass.Crypto (FromByteString (fromByteString), Hexadecimal)
import WBPS.Adapter.Math.AffinePoint (parseIntegerValue)
import WBPS.Adapter.Math.Integer qualified as AdapterInteger

newtype CommitmentId = CommitmentId {unComId :: Hexadecimal}
  deriving newtype (Eq, Ord, Show, IsString, FromJSON, ToJSON)

newtype MessageLimbs = MessageLimbs
  { unMessageLimbs :: [Integer]
  }
  deriving newtype (Eq, Show)

instance ToJSON MessageLimbs where
  toJSON (MessageLimbs limbs) = toJSON (map AdapterInteger.toValue limbs)

instance FromJSON MessageLimbs where
  parseJSON = withArray "MessageLimbs" $ \arr -> do
    limbs <- traverse parseIntegerValue (V.toList arr) :: Parser [Integer]
    pure (MessageLimbs limbs)

newtype CommitmentPayload = CommitmentPayload
  { unPayload :: MessageLimbs
  }
  deriving newtype (Eq, Show, FromJSON, ToJSON)

data Commitment
  = Commitment {id :: CommitmentId, payload :: CommitmentPayload}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

mkCommitment :: CommitmentPayload -> Commitment
mkCommitment payload =
  let
    -- Encode each limb as big-endian bytes with length prefix
    -- It takes the [Integer] payload limbs and encodes each as big-endian
    -- bytes with a 1-byte length prefix (len || limb_bytes), concatenates all of them, then hashes
    -- with SHA-256 to get CommitmentId. This is a simple, deterministic framing to avoid ambiguity
    -- between limbs like [1, 23] vs [12, 3], but it's not CBOR and there's no domain separation
    -- or explicit length for the whole list.
    encodeLimb n =
      let bytes = integerToWords n
       in BS.cons (fromIntegral (length bytes)) (BS.pack bytes)
    MessageLimbs limbs = unPayload payload
    flat = BS.concat (map encodeLimb limbs)
    digest = hash @_ @SHA256 flat
    commitmentId = CommitmentId . fromByteString @Hexadecimal $ (BA.convert digest :: ByteString)
   in
    Commitment {id = commitmentId, payload}

commitmentPayloadToText :: CommitmentPayload -> [Text]
commitmentPayloadToText (CommitmentPayload (MessageLimbs limbs)) =
  map AdapterInteger.toText limbs

integerToWords :: Integer -> [Word8]
integerToWords = AdapterInteger.toBytesBigEndian
