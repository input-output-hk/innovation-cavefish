{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WBPS.Core.Keys.ElGamal (
  KeyPair (..),
  AffinePoint (..),
  EncryptionKey (..),
  DecryptionKey (..),
  Rho,
  generateKeyPair,
  encryptionKeyPowRho,
  generatorPowRho,
  generateElGamalExponent,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Crypto.Number.ModArithmetic (inverse)
import Crypto.Random (getRandomBytes)
import Data.Aeson (
  FromJSON (..),
  ToJSON (..),
  Value (Array, Number, Object, String),
  eitherDecode,
  (.:),
 )
import Data.Aeson.Types (Parser, typeMismatch)
import Data.Bits (shiftL, shiftR, testBit)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Scientific (floatingOrInteger)
import Data.Text qualified as T
import Data.Vector qualified as V
import GHC.Generics (Generic)
import System.Process (readProcess)
import Text.Read (readMaybe)

newtype Rho = Rho Integer

generateElGamalExponent :: MonadIO m => m Rho
generateElGamalExponent = liftIO (Rho . bsToInteger <$> getRandomBytes 16)

-- | Affine point on BabyJub.
data AffinePoint = AffinePoint {x :: Integer, y :: Integer}
  deriving (Eq, Show, Generic, Ord)

-- | Public encryption key (X = x·G)
newtype EncryptionKey = EncryptionKey {unEncryptionKey :: AffinePoint}
  deriving (Eq, Show, Generic, Ord)

-- | Secret decryption key (scalar seed x)
newtype DecryptionKey = DecryptionKey Integer
  deriving (Eq, Show, Ord)

data KeyPair = KeyPair
  { ek :: EncryptionKey
  , dk :: DecryptionKey
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

instance ToJSON AffinePoint where
  toJSON (AffinePoint x y) = toJSON (fmap integerValue [x, y])

instance FromJSON AffinePoint where
  parseJSON = \case
    Array arr ->
      case V.toList arr of
        [vx, vy] -> AffinePoint <$> parseIntegerValue vx <*> parseIntegerValue vy
        _ -> fail "AffinePoint: expected array with exactly two elements"
    Object o ->
      AffinePoint
        <$> (o .: "x" >>= parseIntegerValue)
        <*> (o .: "y" >>= parseIntegerValue)
    v -> typeMismatch "AffinePoint" v

instance ToJSON EncryptionKey where
  toJSON (EncryptionKey p) = toJSON p

instance FromJSON EncryptionKey where
  parseJSON v = EncryptionKey <$> parseJSON v

instance ToJSON DecryptionKey where
  toJSON (DecryptionKey n) = integerValue n

instance FromJSON DecryptionKey where
  parseJSON = fmap DecryptionKey . parseIntegerValue

generateKeyPair :: forall m. MonadIO m => m KeyPair
generateKeyPair = liftIO $ do
  output <- readProcess "babyjubjub-keygen" [] ""
  case eitherDecode (BL8.pack output) of
    Left err -> fail ("babyjubjub-keygen: failed to decode JSON: " <> err)
    Right kp -> pure kp

-- | Multiply an encryption key (affine BabyJub point) by the scalar ρ.
--   ρ must satisfy 0 ≤ ρ < 2^251 to mirror the circuit constraint.
encryptionKeyPowRho :: EncryptionKey -> Rho -> Either String AffinePoint
encryptionKeyPowRho point (Rho rho) = do
  scalar <- validateScalar rho
  affine <- ensureOnCurve point
  scalarMul affine scalar

-- | Multiply the fixed generator g by ρ (same constraint as 'encryptionKeyPow').
generatorPowRho :: Rho -> Either String AffinePoint
generatorPowRho = encryptionKeyPowRho (EncryptionKey generatorPoint)

scalarMul :: EncryptionKey -> Integer -> Either String AffinePoint
scalarMul point scalar =
  affineFromProjective $
    go scalar identity (toProjective point)
  where
    identity = PointProjective 0 1 1

    go 0 acc _ = acc
    go n acc dbl =
      let acc' = if testBit n 0 then addProjective acc dbl else acc
          dbl' = addProjective dbl dbl
       in go (shiftR n 1) acc' dbl'

data PointProjective = PointProjective
  { px :: Integer
  , py :: Integer
  , pz :: Integer
  }

toProjective :: EncryptionKey -> PointProjective
toProjective (EncryptionKey AffinePoint {x, y}) =
  PointProjective (modQ x) (modQ y) 1

addProjective :: PointProjective -> PointProjective -> PointProjective
addProjective PointProjective {px = x1, py = y1, pz = z1} PointProjective {px = x2, py = y2, pz = z2} =
  PointProjective x3 y3 z3
  where
    a = mul z1 z2
    b = square a
    c = mul x1 x2
    d = mul y1 y2
    e = mul curveD (mul c d)
    f = sub b e
    g = add b e
    x1y1 = add x1 y1
    x2y2 = add x2 y2
    aux = sub (sub (mul x1y1 x2y2) c) d
    x3 = mul (mul a f) aux
    ac = mul curveA c
    dac = sub d ac
    y3 = mul (mul a g) dac
    z3 = mul f g

affineFromProjective :: PointProjective -> Either String AffinePoint
affineFromProjective PointProjective {px, py, pz} =
  case inverse (modQ pz) babyJubPrime of
    Nothing -> Left "encountered point at infinity when normalising BabyJub point"
    Just zInv ->
      Right
        AffinePoint
          { x = mul px zInv
          , y = mul py zInv
          }

ensureOnCurve :: EncryptionKey -> Either String EncryptionKey
ensureOnCurve (EncryptionKey AffinePoint {x, y})
  | lhs == rhs = Right (EncryptionKey (AffinePoint x' y'))
  | otherwise = Left "encryption key is not on the BabyJub curve"
  where
    x' = modQ x
    y' = modQ y
    x2 = square x'
    y2 = square y'
    lhs = modQ (curveA * x2 + y2)
    rhs = modQ (1 + curveD * x2 * y2)

validateScalar :: Integer -> Either String Integer
validateScalar rho
  | rho < 0 = Left "ρ must be non-negative"
  | rho >= scalarLimit = Left "ρ must fit in 251 bits (ρ < 2^251)"
  | otherwise = Right rho

modQ :: Integer -> Integer
modQ n = n `mod` babyJubPrime

add :: Integer -> Integer -> Integer
add a b = modQ (a + b)

sub :: Integer -> Integer -> Integer
sub a b = modQ (a - b)

mul :: Integer -> Integer -> Integer
mul a b = modQ (a * b)

square :: Integer -> Integer
square a = mul a a

babyJubPrime :: Integer
babyJubPrime = 21888242871839275222246405745257275088548364400416034343698204186575808495617

curveA :: Integer
curveA = 168700

curveD :: Integer
curveD = 168696

scalarLimit :: Integer
scalarLimit = 1 `shiftL` 251

-- | Interpret a ByteString as a big-endian unsigned integer.
bsToInteger :: ByteString -> Integer
bsToInteger = BS.foldl' (\acc b -> acc * 256 + fromIntegral b) 0

generatorPoint :: AffinePoint
generatorPoint =
  AffinePoint
    { x = 5299619240641551281634865583518297030282874472190772894086521144482721001553
    , y = 16950150798460657717958625567821834550301663161624707787222815936182638968203
    }

integerValue :: Integer -> Value
integerValue = String . T.pack . show

parseIntegerValue :: Value -> Parser Integer
parseIntegerValue = \case
  String t ->
    maybe (fail "expected integer encoded as decimal string") pure (readMaybe (T.unpack t))
  Number n ->
    case floatingOrInteger n of
      Right i -> pure i
      Left (_ :: Double) -> fail "expected integral number, got fractional"
  v -> typeMismatch "integer (string or number)" v
