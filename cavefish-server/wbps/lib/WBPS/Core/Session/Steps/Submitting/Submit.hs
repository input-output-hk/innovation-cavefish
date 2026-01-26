module WBPS.Core.Session.Steps.Submitting.Submit (
  submit,
) where

import Cardano.Api qualified as Api
import Cardano.Crypto.DSIGN (Ed25519DSIGN, SigDSIGN, rawDeserialiseSigDSIGN)
import Cardano.Crypto.DSIGN.Class qualified as DSIGN
import Cardano.Ledger.Keys qualified as LedgerKeys
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import WBPS.Adapter.CardanoCryptoClass.Crypto qualified as Adapter
import WBPS.Core.Failure (WBPSFailure (SessionSubmittingFailed))
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (UserWalletPublicKey (UserWalletPublicKey))
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 qualified as Ed25519
import WBPS.Core.Registration.Registered (Registered)
import WBPS.Core.Registration.RegistrationId (RegistrationId (RegistrationId, userWalletPublicKey))
import WBPS.Core.Session.SessionId (SessionId (SessionId, registrationId))
import WBPS.Core.Session.Steps.BlindSigning.BlindSignature (
  BlindSignature,
  assertBlindSignatureEquationHolds,
  signatureBytes,
 )
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Cardano.UnsignedTx (UnsignedTx)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.PreparedMessage (
  Message (Message),
  MessageParts (MessageParts, message),
  PreparedMessage (PreparedMessage, parts),
 )
import WBPS.Core.Session.Steps.Demonstration.Artefacts.R (R)
import WBPS.Core.Session.Steps.Demonstration.Demonstrated (CommitmentDemonstrated (CommitmentDemonstrated, preparedMessage))
import WBPS.Core.Session.Steps.Proving.Artefacts.Challenge (Challenge)
import WBPS.Core.Session.Steps.Proving.Persistence.Events qualified as Proved
import WBPS.Core.Session.Steps.Proving.Proved (CommitmentProved (CommitmentProved, bigR, challenge))
import WBPS.Core.Session.Steps.Submitting.Artefacts.SubmittedTx (
  SubmitTx,
  SubmittedTx (SubmittedTx),
  getTxId,
  mkSignedTx,
  unSignedTx,
 )
import WBPS.Core.Session.Steps.Submitting.Artefacts.TxSignature (TxSignature (TxSignature))
import WBPS.Core.Session.Steps.Submitting.Persistence.Events qualified as Submitted
import WBPS.Core.Session.Steps.Submitting.Submitted (
  CommitmentSubmitted (CommitmentSubmitted, blindSignature, submittedTx, txId, txSignature),
 )
import WBPS.Core.Setup.Circuit.FileScheme (FileScheme)

submit ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  SubmitTx m ->
  SessionId ->
  BlindSignature ->
  m CommitmentSubmitted
submit submitTx sessionId@SessionId {registrationId = RegistrationId {userWalletPublicKey = bigX}} blindSignature = do
  (registered, demonstrated, challenge, bigR, unsignedTx) <- project sessionId
  -- Accept signature (R,s) only when g^s = R * X^c.
  assertBlindSignatureEquationHolds bigR bigX challenge blindSignature
  txSignature <- unblind bigX blindSignature
  signedTx <- mkSignedTx txSignature unsignedTx
  submitTx signedTx
  Submitted.persist
    registered
    demonstrated
    CommitmentSubmitted
      { blindSignature
      , txSignature
      , submittedTx = SubmittedTx (unSignedTx signedTx)
      , txId = getTxId signedTx
      }

project ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  SessionId ->
  m (Registered, CommitmentDemonstrated, Challenge, R, UnsignedTx)
project sessionId =
  Proved.loadHistory sessionId
    >>= \Proved.EventHistory
           { registered
           , demonstrated =
             demonstrated@CommitmentDemonstrated
               { preparedMessage =
                 PreparedMessage
                   { parts =
                     MessageParts
                       { message = Message unsignedTx
                       }
                   }
               }
           , proved = CommitmentProved {challenge, bigR}
           } -> return (registered, demonstrated, challenge, bigR, unsignedTx)

unblind ::
  MonadError [WBPSFailure] m =>
  UserWalletPublicKey ->
  BlindSignature ->
  m TxSignature
unblind userWalletPublicKey blindSignature = do
  sig <- decodeSignature blindSignature
  let vkey = toLedgerVKey userWalletPublicKey
      witness = Api.ShelleyKeyWitness Api.ShelleyBasedEraConway (LedgerKeys.WitVKey vkey (DSIGN.SignedDSIGN sig))
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
      throwError [SessionSubmittingFailed "Signature bytes failed Ed25519 deserialisation."]
    Just sig -> pure sig
