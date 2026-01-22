module WBPS.Core.Session.Steps.Submitting.Submit (
  submit,
) where

import Cardano.Api qualified as Api
import Cardano.Crypto.DSIGN (Ed25519DSIGN, SigDSIGN, VerKeyDSIGN, rawDeserialiseSigDSIGN, rawDeserialiseVerKeyDSIGN)
import Cardano.Crypto.DSIGN.Class qualified as DSIGN
import Cardano.Ledger.Keys qualified as LedgerKeys
import Control.Monad (unless)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Crypto.ECC.Edwards25519 (Point, Scalar, pointAdd, pointDecode, pointMul, scalarDecodeLong, toPoint)
import Crypto.Error (CryptoFailable (CryptoFailed, CryptoPassed))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import WBPS.Adapter.CardanoCryptoClass.Crypto qualified as Adapter
import WBPS.Core.Failure (WBPSFailure (BlindSignatureFailed))
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (UserWalletPublicKey (UserWalletPublicKey))
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 qualified as Ed25519
import WBPS.Core.Registration.RegistrationId (RegistrationId (RegistrationId, userWalletPublicKey))
import WBPS.Core.Session.SessionId (SessionId (SessionId, registrationId))
import WBPS.Core.Session.Steps.BlindSigning.BlindSignature (BlindSignature, signatureBytes)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Cardano.UnsignedTx (UnsignedTx (UnsignedTx))
import WBPS.Core.Session.Steps.Demonstration.Artefacts.PreparedMessage (
  CircuitMessage (CircuitMessage, message),
  Message (Message),
  MessageParts (MessageParts, message),
  PreparedMessage (PreparedMessage, circuit, parts),
 )
import WBPS.Core.Session.Steps.Demonstration.Artefacts.R (R (R))
import WBPS.Core.Session.Steps.Demonstration.Demonstrated (CommitmentDemonstrated (CommitmentDemonstrated, preparedMessage))
import WBPS.Core.Session.Steps.Proving.Artefacts.Challenge (Challenge)
import WBPS.Core.Session.Steps.Proving.Artefacts.Challenge qualified as Challenge
import WBPS.Core.Session.Steps.Proving.Persistence.Events qualified as Proved
import WBPS.Core.Session.Steps.Submitting.Artefacts.SubmittedTx (SubmittedTx (SubmittedTx))
import WBPS.Core.Session.Steps.Submitting.Artefacts.TxSignature (TxSignature (TxSignature))
import WBPS.Core.Session.Steps.Submitting.Persistence.Events qualified as Submitted
import WBPS.Core.Session.Steps.Submitting.Submitted (
  CommitmentSubmitted (CommitmentSubmitted, blindSignature, submittedTx, txId, txSignature),
 )
import WBPS.Core.Setup.Circuit.FileScheme (FileScheme)

submit ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  (Api.Tx Api.ConwayEra -> m ()) ->
  SessionId ->
  BlindSignature ->
  m CommitmentSubmitted
submit submitTx sessionId@SessionId {registrationId = RegistrationId {userWalletPublicKey}} signature = do
  Proved.EventHistory
    { registered
    , demonstrated =
      demonstrated@CommitmentDemonstrated
        { preparedMessage = PreparedMessage {circuit = CircuitMessage {message = messageBits}, parts = MessageParts {message = Message (UnsignedTx txBody)}}
        }
    } <-
    Proved.loadHistory sessionId
  bigR <- decodeSignatureR signature
  let challenge = Challenge.compute userWalletPublicKey messageBits bigR
  validateSignature userWalletPublicKey challenge signature
  txSignature <- mkTxSignature userWalletPublicKey signature
  let tx = Api.makeSignedTransaction [unTxSignature txSignature] txBody
  submitTx tx
  let submittedTx = SubmittedTx tx
  let txId = Api.getTxId txBody
  let event = CommitmentSubmitted {blindSignature = signature, txSignature, submittedTx, txId}
  Submitted.persist registered demonstrated event

unTxSignature :: TxSignature -> Api.KeyWitness Api.ConwayEra
unTxSignature (TxSignature witness) = witness

decodeSignatureR ::
  MonadError [WBPSFailure] m =>
  BlindSignature ->
  m R
decodeSignatureR signature = do
  (rBytes, _) <- splitSignature signature
  case rawDeserialiseVerKeyDSIGN rBytes :: Maybe (VerKeyDSIGN Ed25519DSIGN) of
    Nothing ->
      throwError [BlindSignatureFailed "Signature R failed Ed25519 deserialisation."]
    Just verKey ->
      pure (R (Ed25519.PublicKey (Adapter.PublicKey verKey)))

splitSignature ::
  MonadError [WBPSFailure] m =>
  BlindSignature ->
  m (ByteString, ByteString)
splitSignature signature =
  let bytes = signatureBytes signature
      bytesLen = BS.length bytes
   in if bytesLen /= signatureSizeBytes
        then throwError [BlindSignatureFailed ("Signature bytes length mismatch: expected 64, got " <> show bytesLen <> ".")]
        else pure (BS.splitAt signatureHalfBytes bytes)

validateSignature ::
  MonadError [WBPSFailure] m =>
  UserWalletPublicKey ->
  Challenge ->
  BlindSignature ->
  m ()
validateSignature registrationId challenge signature = do
  (rBytes, sBytes) <- splitSignature signature
  rPoint <- decodePoint "signature R" rBytes
  xPoint <- decodePoint "signer public key" (publicKeyBytes registrationId)
  cScalar <- decodeScalar "challenge" (BS.pack (Challenge.toWord8s challenge))
  sScalar <- decodeScalar "signature scalar" sBytes
  let left = toPoint sScalar
      right = pointAdd rPoint (pointMul cScalar xPoint)
  unless (left == right) $
    throwError [BlindSignatureFailed "Signature verification failed: Schnorr equation mismatch."]

publicKeyBytes :: UserWalletPublicKey -> ByteString
publicKeyBytes (UserWalletPublicKey (Ed25519.PublicKey pk)) =
  Adapter.toByteString pk

decodePoint ::
  MonadError [WBPSFailure] m =>
  String ->
  ByteString ->
  m Point
decodePoint label bytes =
  case pointDecode bytes of
    CryptoPassed point -> pure point
    CryptoFailed err ->
      throwError [BlindSignatureFailed (label <> ": " <> show err)]

decodeScalar ::
  MonadError [WBPSFailure] m =>
  String ->
  ByteString ->
  m Scalar
decodeScalar label bytes =
  case scalarDecodeLong bytes of
    CryptoPassed scalar -> pure scalar
    CryptoFailed err ->
      throwError [BlindSignatureFailed (label <> ": " <> show err)]

mkTxSignature ::
  MonadError [WBPSFailure] m =>
  UserWalletPublicKey ->
  BlindSignature ->
  m TxSignature
mkTxSignature userWalletPublicKey signature = do
  sig <- decodeSignature signature
  let vkey = toLedgerVKey userWalletPublicKey
  let witness = Api.ShelleyKeyWitness Api.ShelleyBasedEraConway (LedgerKeys.WitVKey vkey (DSIGN.SignedDSIGN sig))
  pure (TxSignature witness)

toLedgerVKey :: UserWalletPublicKey -> LedgerKeys.VKey LedgerKeys.Witness
toLedgerVKey (UserWalletPublicKey (Ed25519.PublicKey (Adapter.PublicKey verKey))) =
  LedgerKeys.VKey verKey

decodeSignature ::
  MonadError [WBPSFailure] m =>
  BlindSignature ->
  m (SigDSIGN Ed25519DSIGN)
decodeSignature signature =
  case rawDeserialiseSigDSIGN (signatureBytes signature) of
    Nothing ->
      throwError [BlindSignatureFailed "Signature bytes failed Ed25519 deserialisation."]
    Just sig -> pure sig

signatureSizeBytes :: Int
signatureSizeBytes = 64

signatureHalfBytes :: Int
signatureHalfBytes = 32
