module WBPS.Core.Session.Steps.Proving.Prove (
  prove,
) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import WBPS.Core.Failure (WBPSFailure)
import WBPS.Core.Registration.Registered (Registered)
import WBPS.Core.Registration.RegistrationId (RegistrationId (RegistrationId, userWalletPublicKey))
import WBPS.Core.Session.SessionId (SessionId (SessionId, registrationId))
import WBPS.Core.Session.Steps.Demonstration.Artefacts.PreparedMessage (
  CircuitMessage (CircuitMessage, message),
  MessageBits,
  PreparedMessage (PreparedMessage, circuit),
 )
import WBPS.Core.Session.Steps.Demonstration.Artefacts.R (R)
import WBPS.Core.Session.Steps.Demonstration.Demonstrated (CommitmentDemonstrated (CommitmentDemonstrated, preparedMessage))
import WBPS.Core.Session.Steps.Demonstration.Persistence.Events qualified as Demonstrated
import WBPS.Core.Session.Steps.Demonstration.Persistence.Events qualified as Previous (EventHistory (EventHistory))
import WBPS.Core.Session.Steps.Proving.Artefacts.Challenge qualified as Challenge
import WBPS.Core.Session.Steps.Proving.Artefacts.Proof.Generate (generateProof)
import WBPS.Core.Session.Steps.Proving.Artefacts.Witness qualified as Witness (generate)
import WBPS.Core.Session.Steps.Proving.Persistence.Events (EventHistory (EventHistory))
import WBPS.Core.Session.Steps.Proving.Proved (CommitmentProved (CommitmentProved, bigR, challenge, proof))
import WBPS.Core.Setup.Circuit.FileScheme (
  FileScheme,
 )

prove ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  SessionId ->
  R ->
  m EventHistory
prove sessionId@SessionId {registrationId = RegistrationId {userWalletPublicKey}} bigR = do
  (registered, demonstrated, messageBits) <- project sessionId
  let challenge = Challenge.compute userWalletPublicKey messageBits bigR
  Witness.generate registered demonstrated bigR challenge
  proof <- generateProof sessionId
  return $
    EventHistory
      registered
      demonstrated
      CommitmentProved {..}

project ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  SessionId ->
  m (Registered, CommitmentDemonstrated, MessageBits)
project sessionId =
  Demonstrated.loadHistory sessionId
    >>= \Previous.EventHistory
           { registered
           , demonstrated =
             demonstrated@CommitmentDemonstrated
               { preparedMessage =
                 PreparedMessage
                   { circuit = CircuitMessage {message = messageBits}
                   }
               }
           } -> return (registered, demonstrated, messageBits)
