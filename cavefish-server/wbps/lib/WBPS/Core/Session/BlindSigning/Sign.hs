{-# LANGUAGE DerivingStrategies #-}

module WBPS.Core.Session.BlindSigning.Sign (
  BlindSignature (..),
  signatureBytes,
  sign,
) where

import Cardano.Crypto.DSIGN (Ed25519DSIGN, SigDSIGN, deriveVerKeyDSIGN, rawDeserialiseSigDSIGN)
import Control.Monad (when)
import Control.Monad.Except (MonadError, throwError)
import Crypto.ECC.Edwards25519 (Scalar, scalarAdd, scalarDecodeLong, scalarEncode, scalarMul)
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
import WBPS.Core.Failure (WBPSFailure (BlindSignatureFailed))
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (KeyPair, PrivateKey, PublicKey)
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 qualified as Ed25519
import WBPS.Core.Session.Demonstration.Artefacts.R (RSecret (RSecret))
import WBPS.Core.Session.Proving.Artefacts.Challenge (Challenge)
import WBPS.Core.Session.Proving.Artefacts.Challenge qualified as Challenge

newtype BlindSignature = BlindSignature {unBlindSignature :: [Word8]}
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON)

signatureBytes :: BlindSignature -> ByteString
signatureBytes (BlindSignature bytes) = BS.pack bytes

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
        throwError [BlindSignatureFailed "KeyPair public key does not match derived private key."]

derivePublicKey :: PrivateKey -> Ed25519.PublicKey
derivePublicKey (Ed25519.PrivateKey (Adapter.PrivateKey sk)) =
  Ed25519.PublicKey (Adapter.PublicKey (deriveVerKeyDSIGN sk))

publicKeyBytes :: PublicKey -> ByteString
publicKeyBytes (Ed25519.PublicKey pk) =
  Adapter.toByteString pk

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
        then throwError [BlindSignatureFailed "Private key is too short to derive a seed."]
        else pure (BS.take seedSize keyBytes)

decodeScalar ::
  MonadError [WBPSFailure] m =>
  ByteString ->
  m Scalar
decodeScalar bytes =
  case scalarDecodeLong bytes of
    CryptoFailed err ->
      throwError [BlindSignatureFailed ("Failed to decode scalar: " <> show err)]
    CryptoPassed scalar -> pure scalar

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
      throwError [BlindSignatureFailed "Signature bytes failed Ed25519 deserialisation."]
    Just _ -> pure ()

seedSize :: Int
seedSize = 32
