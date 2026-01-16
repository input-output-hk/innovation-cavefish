module WBPS.Core.Session.Proving.Prove (
  prove,
) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import WBPS.Core.Failure (WBPSFailure)
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Session.Demonstration.Artefacts.Commitment (CommitmentId)
import WBPS.Core.Session.Demonstration.Artefacts.PreparedMessage (
  CircuitMessage (CircuitMessage, message),
  PreparedMessage (PreparedMessage, circuit),
 )
import WBPS.Core.Session.Demonstration.Artefacts.R (R)
import WBPS.Core.Session.Demonstration.Demonstrated (CommitmentDemonstrated (CommitmentDemonstrated, preparedMessage))
import WBPS.Core.Session.FetchSession (loadExistingCommitmentDemonstrationEvents)
import WBPS.Core.Session.Proving.Artefacts.Challenge qualified as Challenge
import WBPS.Core.Session.Proving.Artefacts.Proof.Generate (generateProof)
import WBPS.Core.Session.Proving.Artefacts.Witness qualified as Witness (generate)
import WBPS.Core.Session.Proving.Proved (CommitmentProved (CommitmentProved, bigR, challenge, proof))
import WBPS.Core.Setup.Circuit.FileScheme (
  FileScheme,
 )

prove ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  UserWalletPublicKey ->
  CommitmentId ->
  R ->
  m CommitmentProved
prove userWalletPublicKey commitmentId bigR = do
  (registered, commitmentDemonstrated@CommitmentDemonstrated {preparedMessage = PreparedMessage {circuit = CircuitMessage {message = messageBits}}}) <-
    loadExistingCommitmentDemonstrationEvents userWalletPublicKey commitmentId
  let challenge = Challenge.compute userWalletPublicKey messageBits bigR
  Witness.generate registered commitmentDemonstrated bigR challenge
  proof <- generateProof userWalletPublicKey commitmentId
  return CommitmentProved {..}
