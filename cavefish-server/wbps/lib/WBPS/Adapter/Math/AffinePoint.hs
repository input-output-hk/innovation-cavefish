module WBPS.Adapter.Math.AffinePoint (
  AffinePoint (..),
  toText,
  toBits,
  parseIntegerValue,
) where

import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toJSON),
  Value (Array, Number, Object, String),
  (.:),
 )
import Data.Aeson.Types (Parser, typeMismatch)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as V
import Data.Word (Word8)
import GHC.Generics (Generic)
import Text.Read (readMaybe)
import WBPS.Adapter.Math.Integer qualified as Integer

data AffinePoint = AffinePoint {x :: Integer, y :: Integer}
  deriving (Eq, Show, Generic, Ord)

instance ToJSON AffinePoint where
  toJSON (AffinePoint x y) = toJSON (fmap Integer.toValue [x, y])

instance FromJSON AffinePoint where
  parseJSON v = case v of
    Array arr ->
      case V.toList arr of
        [vx, vy] -> AffinePoint <$> parseIntegerValue vx <*> parseIntegerValue vy
        _ -> fail "AffinePoint: expected array with exactly two elements"
    Object o ->
      AffinePoint
        <$> (o .: "x" >>= parseIntegerValue)
        <*> (o .: "y" >>= parseIntegerValue)
    other -> typeMismatch "AffinePoint" other

toText :: AffinePoint -> [Text]
toText AffinePoint {x, y} = [Integer.toText x, Integer.toText y]

toBits :: AffinePoint -> [Word8]
toBits AffinePoint {x} = Integer.toBitsLittleEndianFixedBytes pointBytes x

parseIntegerValue :: Value -> Parser Integer
parseIntegerValue v = case v of
  String t ->
    maybe (fail "expected integer encoded as decimal string") pure (readMaybe (Text.unpack t))
  Number n ->
    case (floatingOrInteger n :: Either Double Integer) of
      Right i -> pure i
      Left _ -> fail "expected integral number, got fractional"
  other -> typeMismatch "integer (string or number)" other

pointBytes :: Int
pointBytes = 32
