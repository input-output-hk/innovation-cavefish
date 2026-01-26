{-# LANGUAGE DerivingStrategies #-}

module WBPS.Core.Session.Steps.BlindSigning.BlindSignature (
  BlindSignature (..),
  assertBlindSignatureEquationHolds,
  signatureBytes,
  sign,
) where

import Cardano.Crypto.DSIGN (Ed25519DSIGN, SigDSIGN, deriveVerKeyDSIGN, rawDeserialiseSigDSIGN)
import Control.Monad (when)
import Control.Monad.Except (MonadError, throwError)
import Crypto.ECC.Edwards25519 (
  Point,
  Scalar,
  pointAdd,
  pointDecode,
  pointMul,
  scalarAdd,
  scalarDecodeLong,
  scalarEncode,
  scalarMul,
  toPoint,
 )
import Crypto.Error (CryptoFailable (CryptoFailed, CryptoPassed))
import Crypto.Hash (Digest, SHA512, hash)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bits ((.&.), (.|.))
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word8)
import GHC.Generics (Generic)
import WBPS.Adapter.CardanoCryptoClass.Crypto qualified as Adapter
import WBPS.Core.Failure (WBPSFailure (SessionSubmittingFailed))
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (KeyPair, PrivateKey, PublicKey, UserWalletPublicKey (UserWalletPublicKey))
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 qualified as Ed25519
import WBPS.Core.Session.Steps.Demonstration.Artefacts.R (R (R), RSecret (RSecret))
import WBPS.Core.Session.Steps.Proving.Artefacts.Challenge (Challenge)
import WBPS.Core.Session.Steps.Proving.Artefacts.Challenge qualified as Challenge

newtype BlindSignature = BlindSignature {unBlindSignature :: [Word8]}
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

signatureBytes :: BlindSignature -> ByteString
signatureBytes (BlindSignature bytes) = BS.pack bytes

-- | Assert the Schnorr verification equation holds for (R, s) with public key X.
--   This checks g^s = R * X^c for the transcript challenge c.
assertBlindSignatureEquationHolds ::
  MonadError [WBPSFailure] m =>
  R ->
  UserWalletPublicKey ->
  Challenge ->
  BlindSignature ->
  m ()
assertBlindSignatureEquationHolds bigR bigX challenge signature = do
  (rBytes, sBytes) <- splitSignature signature
  let expectedRBytes = rBytesFromR bigR
  when (rBytes /= expectedRBytes) $
    throwError [SessionSubmittingFailed "Blind signature invalid: R does not match the committed nonce."]
  nonceCommitment <- decodePoint "nonce commitment R" rBytes
  signerPublicKeyPoint <- decodePoint "signer public key X" (userWalletPublicKeyBytes bigX)
  transcriptChallenge <- decodeScalarWithLabel "challenge c" (BS.pack (Challenge.toWord8s challenge))
  signatureResponse <- decodeScalarWithLabel "signature response s" sBytes
  let gToS = toPoint signatureResponse
      rTimesXToC = pointAdd nonceCommitment (pointMul transcriptChallenge signerPublicKeyPoint)
  when (gToS /= rTimesXToC) $
    throwError [SessionSubmittingFailed "Blind signature invalid: expected g^s = R * X^c."]

-- | User-side Schnorr response for the WBPS protocol.
sign ::
  MonadError [WBPSFailure] m =>
  KeyPair ->
  RSecret ->
  Challenge ->
  m BlindSignature
sign keyPair (RSecret noncePrivateKey) challenge = do
  assertKeyPairMatches keyPair
  buildBlindSignature (Ed25519.getPrivateKey keyPair) noncePrivateKey challenge

buildBlindSignature ::
  MonadError [WBPSFailure] m =>
  PrivateKey ->
  PrivateKey ->
  Challenge ->
  m BlindSignature
buildBlindSignature userPrivateKey noncePrivateKey challenge = do
  userScalar <- privateKeyToScalar userPrivateKey
  nonceScalar <- privateKeyToScalar noncePrivateKey
  challengeScalar <- challengeToScalar challenge
  let sScalar = scalarAdd nonceScalar (scalarMul challengeScalar userScalar)
      rBytes = publicKeyBytes (derivePublicKey noncePrivateKey)
      sBytes = scalarEncode sScalar :: ByteString
      signature = rBytes <> sBytes
  ensureValidSignature signature
  pure (BlindSignature (BS.unpack signature))

assertKeyPairMatches ::
  MonadError [WBPSFailure] m =>
  KeyPair ->
  m ()
assertKeyPairMatches keyPair =
  let derived = derivePublicKey (Ed25519.getPrivateKey keyPair)
      expected = Ed25519.getPublicKey keyPair
   in when (derived /= expected) $
        throwError [SessionSubmittingFailed "KeyPair public key does not match derived private key."]

derivePublicKey :: PrivateKey -> Ed25519.PublicKey
derivePublicKey (Ed25519.PrivateKey (Adapter.PrivateKey sk)) =
  Ed25519.PublicKey (Adapter.PublicKey (deriveVerKeyDSIGN sk))

publicKeyBytes :: PublicKey -> ByteString
publicKeyBytes (Ed25519.PublicKey pk) =
  Adapter.toByteString pk

rBytesFromR :: R -> ByteString
rBytesFromR (R rPk) =
  publicKeyBytes rPk

userWalletPublicKeyBytes :: UserWalletPublicKey -> ByteString
userWalletPublicKeyBytes (UserWalletPublicKey publicKey) =
  publicKeyBytes publicKey

privateKeyToScalar ::
  MonadError [WBPSFailure] m =>
  PrivateKey ->
  m Scalar
privateKeyToScalar privateKey = do
  seed <- seedFromPrivateKey privateKey
  let digest = (hash seed :: Digest SHA512)
      digestBytes = BA.convert digest :: ByteString
  decodeScalar (clampScalarBytes digestBytes)

challengeToScalar ::
  MonadError [WBPSFailure] m =>
  Challenge ->
  m Scalar
challengeToScalar challenge =
  decodeScalar (BS.pack (Challenge.toWord8s challenge))

seedFromPrivateKey ::
  MonadError [WBPSFailure] m =>
  PrivateKey ->
  m ByteString
seedFromPrivateKey (Ed25519.PrivateKey privateKey) =
  let keyBytes = Adapter.toByteString privateKey
   in if BS.length keyBytes < seedSize
        then throwError [SessionSubmittingFailed "Private key is too short to derive a seed."]
        else pure (BS.take seedSize keyBytes)

decodeScalar ::
  MonadError [WBPSFailure] m =>
  ByteString ->
  m Scalar
decodeScalar bytes =
  case scalarDecodeLong bytes of
    CryptoFailed err ->
      throwError [SessionSubmittingFailed ("Failed to decode scalar: " <> show err)]
    CryptoPassed scalar -> pure scalar

decodeScalarWithLabel ::
  MonadError [WBPSFailure] m =>
  String ->
  ByteString ->
  m Scalar
decodeScalarWithLabel label bytes =
  case scalarDecodeLong bytes of
    CryptoFailed err ->
      throwError [SessionSubmittingFailed (label <> ": " <> show err)]
    CryptoPassed scalar -> pure scalar

decodePoint ::
  MonadError [WBPSFailure] m =>
  String ->
  ByteString ->
  m Point
decodePoint label bytes =
  case pointDecode bytes of
    CryptoPassed point -> pure point
    CryptoFailed err ->
      throwError [SessionSubmittingFailed (label <> ": " <> show err)]

clampScalarBytes :: ByteString -> ByteString
clampScalarBytes bytes =
  let raw = BS.take seedSize bytes
   in if BS.length raw < seedSize
        then raw
        else
          BS.concat
            [ BS.singleton (BS.index raw 0 .&. 248)
            , BS.take (seedSize - 2) (BS.drop 1 raw)
            , BS.singleton ((BS.index raw (seedSize - 1) .&. 63) .|. 64)
            ]

ensureValidSignature ::
  MonadError [WBPSFailure] m =>
  ByteString ->
  m ()
ensureValidSignature signature =
  case rawDeserialiseSigDSIGN signature :: Maybe (SigDSIGN Ed25519DSIGN) of
    Nothing ->
      throwError [SessionSubmittingFailed "Signature bytes failed Ed25519 deserialisation."]
    Just _ -> pure ()

splitSignature ::
  MonadError [WBPSFailure] m =>
  BlindSignature ->
  m (ByteString, ByteString)
splitSignature signature =
  let bytes = signatureBytes signature
      bytesLen = BS.length bytes
   in if bytesLen /= signatureSizeBytes
        then throwError [SessionSubmittingFailed ("Signature bytes length mismatch: expected 64, got " <> show bytesLen <> ".")]
        else pure (BS.splitAt signatureHalfBytes bytes)

seedSize :: Int
seedSize = 32

signatureSizeBytes :: Int
signatureSizeBytes = 64

signatureHalfBytes :: Int
signatureHalfBytes = 32
