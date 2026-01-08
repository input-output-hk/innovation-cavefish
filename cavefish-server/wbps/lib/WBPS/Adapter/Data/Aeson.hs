module WBPS.Adapter.Data.Aeson (
  jsonNumberToText,
) where

import Data.Aeson (ToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding

jsonNumberToText :: ToJSON a => a -> Text
jsonNumberToText =
  TextEncoding.decodeUtf8
    . BL.toStrict
    . Aeson.encode
