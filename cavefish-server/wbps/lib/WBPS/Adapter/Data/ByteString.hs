module WBPS.Adapter.Data.ByteString (
  bytesToBitsLE,
) where

import Data.Bits (testBit)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word8)

bytesToBitsLE :: ByteString -> [Word8]
bytesToBitsLE = concatMap byteToBitsLE . BS.unpack
  where
    byteToBitsLE byte =
      [ if testBit byte i then 1 else 0
      | i <- [0 .. 7]
      ]
