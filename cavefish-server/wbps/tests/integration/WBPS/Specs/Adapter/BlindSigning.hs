module WBPS.Specs.Adapter.BlindSigning (
  blindSignatureProperty,
) where

import Crypto.ECC.Edwards25519 (Point, Scalar, pointAdd, pointDecode, pointMul, scalarDecodeLong, toPoint)
import Crypto.Error (CryptoFailable (CryptoFailed, CryptoPassed))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Path (Dir, Path, Rel)
import Test.QuickCheck (Property, counterexample, ioProperty, (.&&.), (===))
import WBPS.Adapter.CardanoCryptoClass.Crypto qualified as Adapter
import WBPS.Core.Failure (WBPSFailure)
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (KeyPair, PublicKey (PublicKey), generateKeyPair, getPublicKey, userWalletPK)
import WBPS.Core.Registration.Register (register)
import WBPS.Core.Registration.RegistrationId (RegistrationId (RegistrationId))
import WBPS.Core.Session.Steps.BlindSigning.BlindSignature (BlindSignature, sign, signatureBytes)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Cardano.UnsignedTx (UnsignedTx)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.R (R (R), RSecret, generateKeyTuple)
import WBPS.Core.Session.Steps.Demonstration.Demonstrate (demonstrate)
import WBPS.Core.Session.Steps.Proving.Artefacts.Challenge (Challenge)
import WBPS.Core.Session.Steps.Proving.Artefacts.Challenge qualified as Challenge
import WBPS.Core.Session.Steps.Proving.Persistence.Events qualified as Proved
import WBPS.Core.Session.Steps.Proving.Prove (prove)
import WBPS.Core.Session.Steps.Proving.Proved (CommitmentProved (CommitmentProved, challenge))
import WBPS.Core.Setup.Circuit.FileScheme (FileScheme, defaultFileScheme)
import WBPS.Specs.Adapter.Fixture (CommitmentFixtures (CommitmentFixtures, unsignedTxFixture), commitmentFixtures, readFixture)
import WBPS.Specs.Adapter.Test (getRootFolder)
import WBPS.WBPS (runWBPS)

blindSignatureProperty :: Path Rel Dir -> Property
blindSignatureProperty rootLabel =
  ioProperty $ do
    fixture <- buildFixture rootLabel
    result <- runBlindSigningFlow fixture
    pure $ case result of
      Left failures ->
        counterexample ("Blind signing flow failed: " <> show failures) False
      Right output ->
        blindSignatureLaws output

data BlindSigningFixture = BlindSigningFixture
  { fileScheme :: FileScheme
  , unsignedTx :: UnsignedTx
  , userKeyPair :: KeyPair
  , nonceSecret :: RSecret
  , noncePublic :: R
  }

data BlindSigningResult = BlindSigningResult
  { signature :: BlindSignature
  , challenge :: Challenge
  , userPublicKey :: PublicKey
  , noncePublicKey :: PublicKey
  }

data SignatureParts = SignatureParts
  { signatureRBytes :: ByteString
  , signatureSBytes :: ByteString
  }

buildFixture :: Path Rel Dir -> IO BlindSigningFixture
buildFixture rootLabel = do
  rootFolders <- getRootFolder rootLabel
  let fileScheme = defaultFileScheme rootFolders
      CommitmentFixtures {unsignedTxFixture} = commitmentFixtures rootFolders
  unsignedTx <- readFixture unsignedTxFixture
  userKeyPair <- generateKeyPair
  (nonceSecret, noncePublic) <- generateKeyTuple
  pure
    BlindSigningFixture
      { fileScheme = fileScheme
      , unsignedTx = unsignedTx
      , userKeyPair = userKeyPair
      , nonceSecret = nonceSecret
      , noncePublic = noncePublic
      }

runBlindSigningFlow :: BlindSigningFixture -> IO (Either [WBPSFailure] BlindSigningResult)
runBlindSigningFlow fixture =
  runWBPS fileScheme $ do
    _ <- register userWalletPublicKey
    (sessionId, _) <- demonstrate registrationId unsignedTx
    Proved.EventHistory {proved = CommitmentProved {challenge}} <- prove sessionId noncePublic
    signature <- sign userKeyPair nonceSecret challenge
    pure
      BlindSigningResult
        { signature = signature
        , challenge = challenge
        , userPublicKey = userPublicKey
        , noncePublicKey = noncePublicKey
        }
  where
    BlindSigningFixture {fileScheme, unsignedTx, userKeyPair, nonceSecret, noncePublic} = fixture
    userWalletPublicKey = userWalletPK userKeyPair
    registrationId = RegistrationId userWalletPublicKey
    userPublicKey = getPublicKey userKeyPair
    noncePublicKey = publicKeyFromR noncePublic

blindSignatureLaws :: BlindSigningResult -> Property
blindSignatureLaws BlindSigningResult {signature, challenge, userPublicKey, noncePublicKey} =
  let sigBytes = signatureBytes signature
      SignatureParts {signatureRBytes, signatureSBytes} = splitSignature sigBytes
      nonceBytes = publicKeyBytes noncePublicKey
      lengthCheck =
        counterexample "signature length is 64 bytes" (BS.length sigBytes === signatureSizeBytes)
      nonceCheck =
        counterexample "signature R matches nonce R" (signatureRBytes === nonceBytes)
      schnorrCheck =
        case decodeSignatureInputs signatureRBytes signatureSBytes userPublicKey challenge of
          Left err -> counterexample err False
          Right (rPoint, xPoint, cScalar, sScalar) ->
            let left = toPoint sScalar
                right = pointAdd rPoint (pointMul cScalar xPoint)
             in counterexample "Schnorr equation holds: s*G = R + c*X" (left === right)
   in lengthCheck .&&. nonceCheck .&&. schnorrCheck

splitSignature :: ByteString -> SignatureParts
splitSignature sigBytes =
  let (rBytes, sBytes) = BS.splitAt signatureHalfBytes sigBytes
   in SignatureParts {signatureRBytes = rBytes, signatureSBytes = sBytes}

decodeSignatureInputs ::
  ByteString ->
  ByteString ->
  PublicKey ->
  Challenge ->
  Either String (Point, Point, Scalar, Scalar)
decodeSignatureInputs rBytes sBytes signerPublicKey challenge = do
  rPoint <- decodePoint "signature R" rBytes
  xPoint <- decodePoint "signer public key" (publicKeyBytes signerPublicKey)
  cScalar <- decodeScalar "challenge" (BS.pack (Challenge.toWord8s challenge))
  sScalar <- decodeScalar "signature scalar" sBytes
  pure (rPoint, xPoint, cScalar, sScalar)

publicKeyFromR :: R -> PublicKey
publicKeyFromR (R pk) = pk

publicKeyBytes :: PublicKey -> ByteString
publicKeyBytes (PublicKey pk) = Adapter.toByteString pk

decodePoint :: String -> ByteString -> Either String Point
decodePoint label bytes =
  case pointDecode bytes of
    CryptoPassed point -> Right point
    CryptoFailed err ->
      Left (label <> ": " <> show err)

decodeScalar :: String -> ByteString -> Either String Scalar
decodeScalar label bytes =
  case scalarDecodeLong bytes of
    CryptoPassed scalar -> Right scalar
    CryptoFailed err ->
      Left (label <> ": " <> show err)

signatureSizeBytes :: Int
signatureSizeBytes = 64

signatureHalfBytes :: Int
signatureHalfBytes = 32
