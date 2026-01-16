module WBPS.Adapter.Data.Word8 (
  word8ToText,
  word8sToText,
) where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word8)

word8ToText :: Word8 -> Text
word8ToText = Text.pack . show

word8sToText :: [Word8] -> [Text]
word8sToText = map word8ToText
