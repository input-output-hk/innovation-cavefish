module WBPS.Adapter.Math.Integer (
  toText,
  toValue,
  toBitsLEFixedBytes,
  toBytesBE,
) where

import Data.Aeson (Value (String))
import Data.Bits (testBit)
import Data.List (unfoldr)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word8)

toText :: Integer -> Text
toText = Text.pack . show

toValue :: Integer -> Value
toValue = String . toText

toBitsLEFixedBytes :: Int -> Integer -> [Word8]
toBitsLEFixedBytes byteLen value =
  let bytes = toBytesBE value
      padded =
        if length bytes < byteLen
          then replicate (byteLen - length bytes) 0 <> bytes
          else bytes
   in concatMap byteToBitsLE padded
  where
    byteToBitsLE byte =
      [ if testBit byte i then 1 else 0
      | i <- [0 .. 7]
      ]

toBytesBE :: Integer -> [Word8]
toBytesBE 0 = [0]
toBytesBE n
  | n < 0 = error "toBytesBE: negative input"
  | otherwise = reverse (unfoldr step n)
  where
    step 0 = Nothing
    step k =
      let (q, r) = k `quotRem` 256
       in Just (fromIntegral r, q)
