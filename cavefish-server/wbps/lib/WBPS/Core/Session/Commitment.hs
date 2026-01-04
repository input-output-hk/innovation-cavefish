{-# LANGUAGE OverloadedStrings #-}

module WBPS.Core.Session.Commitment (
  mkCommitment,
  Commitment (..),
  CommitmentPayload (..),
  CommitmentId (..),
) where

import Cardano.Crypto.Hash (ByteString)
import Crypto.Hash (SHA256, hash)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteArray qualified as BA
import Data.ByteString qualified as BS
import Data.Coerce (coerce)
import Data.List (unfoldr)
import Data.String (IsString)
import Data.Word (Word8)
import GHC.Generics (Generic)
import WBPS.Adapter.CardanoCryptoClass.Crypto (FromByteString (fromByteString), Hexadecimal)
import WBPS.Core.ZK.Message (
  MessageBits (MessageBits),
 )

newtype CommitmentId = CommitmentId {unComId :: Hexadecimal}
  deriving newtype (Eq, Ord, Show, IsString, FromJSON, ToJSON)

newtype CommitmentPayload = CommitmentPayload
  { unPayload :: MessageBits
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
      let bs = integerToBytes n
       in BS.cons (fromIntegral (length bs)) (BS.pack bs)
    flat = BS.concat (map encodeLimb (coerce payload))
    digest = hash @_ @SHA256 flat
    commitmentId = CommitmentId . fromByteString @Hexadecimal $ (BA.convert digest :: ByteString)
   in
    Commitment {id = commitmentId, payload}

integerToBytes :: Integer -> [Word8]
integerToBytes 0 = [0]
integerToBytes n
  | n < 0 = error "integerToBytes: negative input"
  | otherwise = reverse (unfoldr step n)
  where
    step 0 = Nothing
    step k =
      let (q, r) = k `quotRem` 256
       in Just (fromIntegral r, q)
