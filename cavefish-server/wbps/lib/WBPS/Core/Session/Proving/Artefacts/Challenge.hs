{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Compute the Schnorr challenge that the circuit labels P3.
--   This mirrors RebuildChallenge in `wbps/circuits/wbps_cardano.circom`:
--     c = SHA-512(R || X || mu_bits)
--   where R and X are Ed25519 public keys (per-byte bit-reversed in-circuit)
--   and mu_bits are the fixed-length message bits (LSB-first per byte).
module WBPS.Core.Session.Proving.Artefacts.Challenge (
  Challenge (..),
  compute,
  computeByUsingTxId,
  toWord8s,
) where

import Cardano.Api qualified as Api
import Crypto.Hash (Digest, SHA512, digestFromByteString)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), withArray)
import Data.Aeson.Types (Parser)
import Data.Bits (complement, rotateR, shiftL, shiftR, testBit, xor, (.&.), (.|.))
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List (foldl')
import Data.Vector qualified as V
import Data.Word (Word64, Word8)
import WBPS.Adapter.CardanoCryptoClass.Crypto qualified as Crypto
import WBPS.Adapter.Data.ByteString (bytesToBitsLE)
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (PublicKey (PublicKey), UserWalletPublicKey (UserWalletPublicKey))
import WBPS.Core.Session.Demonstration.Artefacts.Cardano.UnsignedTx (UnsignedTx (txUnsigned))
import WBPS.Core.Session.Demonstration.Artefacts.PreparedMessage (
  Message (Message),
  MessageBits (MessageBits),
 )
import WBPS.Core.Session.Demonstration.Artefacts.R (R (R))

-- | Challenge digest used by the transcript.
newtype Challenge = Challenge (Digest SHA512)
  deriving (Eq, Show)

instance ToJSON Challenge where
  toJSON (Challenge digest) =
    toJSON (BS.unpack (BA.convert digest :: ByteString))

instance FromJSON Challenge where
  parseJSON = withArray "Challenge" $ \arr -> do
    bytes <- traverse parseJSON (V.toList arr) :: Parser [Word8]
    case digestFromByteString (BS.pack bytes) of
      Nothing -> fail "Challenge: invalid digest length"
      Just digest -> pure (Challenge digest)

-- | Recompute the transcript challenge as done in the circuit.
--   The message is first expanded to the fixed circuit length via 'toBitsPaddedToMaxSize',
--   then its bytes are bit-reversed to match the circuit's LSB-first bit ordering.
compute :: UserWalletPublicKey -> MessageBits -> R -> Challenge
compute (UserWalletPublicKey userPk) (MessageBits messageBits) (R rPk) =
  Challenge (digestFromBitsOrFail (rBits ++ xBits ++ BS.unpack messageBits))
  where
    rBits = reverseBitsPerByte (bytesToBitsLE (publicKeyBytes rPk))
    xBits = reverseBitsPerByte (bytesToBitsLE (publicKeyBytes userPk))

-- | Recompute the transcript challenge using the transaction id instead of the
--   full message bits. The tx id bytes are bit-reversed per byte to match the
--   circuit's LSB-first bit ordering.
computeByUsingTxId :: UserWalletPublicKey -> Message -> R -> Challenge
computeByUsingTxId (UserWalletPublicKey userPk) message (R rPk) =
  Challenge (digestFromBitsOrFail (rBits ++ xBits ++ txIdBits))
  where
    rBits = reverseBitsPerByte (bytesToBitsLE (publicKeyBytes rPk))
    xBits = reverseBitsPerByte (bytesToBitsLE (publicKeyBytes userPk))
    txIdBytes = txIdFromMessage message
    txIdBits = reverseBitsPerByte (bytesToBitsLE txIdBytes)

toWord8s :: Challenge -> [Word8]
toWord8s (Challenge digest) =
  BS.unpack (BA.convert digest :: ByteString)

publicKeyBytes :: PublicKey -> ByteString
publicKeyBytes (PublicKey pk) =
  Crypto.toByteString pk

txIdFromMessage :: Message -> ByteString
txIdFromMessage (Message unsignedTx) =
  Api.serialiseToRawBytes (Api.getTxId (txUnsigned unsignedTx))

reverseBitsPerByte :: [Word8] -> [Word8]
reverseBitsPerByte bits =
  concatMap reverse (chunksOf 8 bits)

digestFromBitsOrFail :: [Word8] -> Digest SHA512
digestFromBitsOrFail bits =
  case digestFromByteString (BS.pack (sha512BitsDigest bits)) of
    Nothing -> error "Challenge: invalid SHA-512 digest length"
    Just digest -> digest

sha512BitsDigest :: [Word8] -> [Word8]
sha512BitsDigest bits =
  let padded = sha512PadBits bits
      blocks = chunksOf 1024 padded
      final = foldl' sha512Compress sha512IV blocks
   in concatMap word64ToBytesBE final

sha512PadBits :: [Word8] -> [Word8]
sha512PadBits bits =
  let lenBits = length bits
      withOne = bits ++ [1]
      padLen = (896 - (lenBits + 1)) `mod` 1024
      zeros = replicate padLen 0
      lenHi = word64ToBitsBE 0
      lenLo = word64ToBitsBE (fromIntegral lenBits)
   in withOne ++ zeros ++ lenHi ++ lenLo

sha512Compress :: [Word64] -> [Word8] -> [Word64]
sha512Compress h block =
  let w = buildSchedule block
      [a0, b0, c0, d0, e0, f0, g0, h0] = h
      (a, b, c, d, e, f, g, h') = foldl' step (a0, b0, c0, d0, e0, f0, g0, h0) (zip sha512K w)
   in zipWith add64 h [a, b, c, d, e, f, g, h']
  where
    buildSchedule blk = go 16 w0
      where
        w0 = map bitsToWord64BE (chunksOf 64 blk)
        go i ws
          | i >= 80 = ws
          | otherwise =
              let w2 = ws !! (i - 2)
                  w7 = ws !! (i - 7)
                  w15 = ws !! (i - 15)
                  w16 = ws !! (i - 16)
                  w' = add64 (add64 (smallSigma1 w2) w7) (add64 (smallSigma0 w15) w16)
               in go (i + 1) (ws ++ [w'])

    step (a, b, c, d, e, f, g, h') (k, w) =
      let t1 = add64 (add64 (add64 (add64 h' (bigSigma1 e)) (ch e f g)) k) w
          t2 = add64 (bigSigma0 a) (maj a b c)
       in (add64 t1 t2, a, b, c, add64 d t1, e, f, g)

    add64 x y = (x + y) .&. sha512Mask

bitsToWord64BE :: [Word8] -> Word64
bitsToWord64BE =
  foldl' (\acc b -> (acc `shiftL` 1) .|. fromIntegral b) 0

word64ToBitsBE :: Word64 -> [Word8]
word64ToBitsBE w =
  [ if testBit w i then 1 else 0
  | i <- [63, 62 .. 0]
  ]

word64ToBytesBE :: Word64 -> [Word8]
word64ToBytesBE w =
  [ fromIntegral (w `shiftR` 56)
  , fromIntegral (w `shiftR` 48)
  , fromIntegral (w `shiftR` 40)
  , fromIntegral (w `shiftR` 32)
  , fromIntegral (w `shiftR` 24)
  , fromIntegral (w `shiftR` 16)
  , fromIntegral (w `shiftR` 8)
  , fromIntegral w
  ]

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs =
  let (chunk, rest) = splitAt n xs
   in chunk : chunksOf n rest

sha512Mask :: Word64
sha512Mask = 0xFFFFFFFFFFFFFFFF

ch :: Word64 -> Word64 -> Word64 -> Word64
ch x y z = (x .&. y) `xor` (complement x .&. z)

maj :: Word64 -> Word64 -> Word64 -> Word64
maj x y z = (x .&. y) `xor` (x .&. z) `xor` (y .&. z)

bigSigma0 :: Word64 -> Word64
bigSigma0 x = rotateR x 28 `xor` rotateR x 34 `xor` rotateR x 39

bigSigma1 :: Word64 -> Word64
bigSigma1 x = rotateR x 14 `xor` rotateR x 18 `xor` rotateR x 41

smallSigma0 :: Word64 -> Word64
smallSigma0 x = rotateR x 1 `xor` rotateR x 8 `xor` (x `shiftR` 7)

smallSigma1 :: Word64 -> Word64
smallSigma1 x = rotateR x 19 `xor` rotateR x 61 `xor` (x `shiftR` 6)

sha512IV :: [Word64]
sha512IV =
  [ 0x6a09e667f3bcc908
  , 0xbb67ae8584caa73b
  , 0x3c6ef372fe94f82b
  , 0xa54ff53a5f1d36f1
  , 0x510e527fade682d1
  , 0x9b05688c2b3e6c1f
  , 0x1f83d9abfb41bd6b
  , 0x5be0cd19137e2179
  ]

sha512K :: [Word64]
sha512K =
  [ 0x428a2f98d728ae22
  , 0x7137449123ef65cd
  , 0xb5c0fbcfec4d3b2f
  , 0xe9b5dba58189dbbc
  , 0x3956c25bf348b538
  , 0x59f111f1b605d019
  , 0x923f82a4af194f9b
  , 0xab1c5ed5da6d8118
  , 0xd807aa98a3030242
  , 0x12835b0145706fbe
  , 0x243185be4ee4b28c
  , 0x550c7dc3d5ffb4e2
  , 0x72be5d74f27b896f
  , 0x80deb1fe3b1696b1
  , 0x9bdc06a725c71235
  , 0xc19bf174cf692694
  , 0xe49b69c19ef14ad2
  , 0xefbe4786384f25e3
  , 0x0fc19dc68b8cd5b5
  , 0x240ca1cc77ac9c65
  , 0x2de92c6f592b0275
  , 0x4a7484aa6ea6e483
  , 0x5cb0a9dcbd41fbd4
  , 0x76f988da831153b5
  , 0x983e5152ee66dfab
  , 0xa831c66d2db43210
  , 0xb00327c898fb213f
  , 0xbf597fc7beef0ee4
  , 0xc6e00bf33da88fc2
  , 0xd5a79147930aa725
  , 0x06ca6351e003826f
  , 0x142929670a0e6e70
  , 0x27b70a8546d22ffc
  , 0x2e1b21385c26c926
  , 0x4d2c6dfc5ac42aed
  , 0x53380d139d95b3df
  , 0x650a73548baf63de
  , 0x766a0abb3c77b2a8
  , 0x81c2c92e47edaee6
  , 0x92722c851482353b
  , 0xa2bfe8a14cf10364
  , 0xa81a664bbc423001
  , 0xc24b8b70d0f89791
  , 0xc76c51a30654be30
  , 0xd192e819d6ef5218
  , 0xd69906245565a910
  , 0xf40e35855771202a
  , 0x106aa07032bbd1b8
  , 0x19a4c116b8d2d0c8
  , 0x1e376c085141ab53
  , 0x2748774cdf8eeb99
  , 0x34b0bcb5e19b48a8
  , 0x391c0cb3c5c95a63
  , 0x4ed8aa4ae3418acb
  , 0x5b9cca4f7763e373
  , 0x682e6ff3d6b2b8a3
  , 0x748f82ee5defb2fc
  , 0x78a5636f43172f60
  , 0x84c87814a1f0ab72
  , 0x8cc702081a6439ec
  , 0x90befffa23631e28
  , 0xa4506cebde82bde9
  , 0xbef9a3f7b2c67915
  , 0xc67178f2e372532b
  , 0xca273eceea26619c
  , 0xd186b8c721c0c207
  , 0xeada7dd6cde0eb1e
  , 0xf57d4f7fee6ed178
  , 0x06f067aa72176fba
  , 0x0a637dc5a2c898a6
  , 0x113f9804bef90dae
  , 0x1b710b35131c471b
  , 0x28db77f523047d84
  , 0x32caab7b40c72493
  , 0x3c9ebe0a15c9bebc
  , 0x431d67c49c100d4c
  , 0x4cc5d4becb3e42b6
  , 0x597f299cfc657e2a
  , 0x5fcb6fab3ad6faec
  , 0x6c44198c4a475817
  ]
