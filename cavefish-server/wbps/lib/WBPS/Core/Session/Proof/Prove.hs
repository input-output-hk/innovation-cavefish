module WBPS.Core.Session.Proof.Prove (
  prove,
) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader.Class (asks)
import Data.Aeson (Value (Null))
import Path (toFilePath, (</>))
import Shh (Stream (Append, StdOut), (&!>), (&>))
import WBPS.Adapter.Path (readFrom, writeTo)
import WBPS.Core.Failure (RegistrationFailed)
import WBPS.Core.FileScheme
import WBPS.Core.FileScheme qualified as FileScheme
import WBPS.Core.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Primitives.Snarkjs qualified as Snarkjs
import WBPS.Core.Registration.FileScheme (deriveAccountDirectoryFrom)
import WBPS.Core.Session.Challenge (Challenge)
import WBPS.Core.Session.Challenge qualified as Challenge
import WBPS.Core.Session.Commitment (CommitmentId)
import WBPS.Core.Session.FetchSession (loadExistingCommitmentDemonstrationEvents)
import WBPS.Core.Session.FileScheme (deriveExistingSessionDirectoryFrom)
import WBPS.Core.Session.Proof (Proof (Proof))
import WBPS.Core.Session.R (R)
import WBPS.Core.Session.Session (CommitmentDemonstrated (CommitmentDemonstrated, preparedMessage))
import WBPS.Core.Session.Witness qualified as Witness (generate)
import WBPS.Core.ZK.Message (PreparedMessage (PreparedMessage, message))

prove ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  UserWalletPublicKey ->
  CommitmentId ->
  R ->
  m (Challenge, Proof)
prove userWalletPublicKey commitmentId bigR = do
  (accountCreated, commitmentDemonstrated@CommitmentDemonstrated {preparedMessage = PreparedMessage {message}}) <-
    loadExistingCommitmentDemonstrationEvents userWalletPublicKey commitmentId
  let challenge = Challenge.computeByUsingTxId userWalletPublicKey message bigR
  Witness.generate accountCreated commitmentDemonstrated bigR challenge
  proof <- generateProof userWalletPublicKey commitmentId
  pure (challenge, proof)

generateProof ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  UserWalletPublicKey ->
  CommitmentId ->
  m Proof
generateProof userWalletPublicKey commitmentId = do
  sessionDirectory <- deriveExistingSessionDirectoryFrom userWalletPublicKey commitmentId
  accountDirectory <- deriveAccountDirectoryFrom userWalletPublicKey
  Account
    { registration = Registration {provingKey}
    , session =
      Session
        { proving =
          Proving
            { witness = WitnessGeneration {output = witnessOutput}
            , proof = ProofGeneration {proof = proofOutput, statement = statementOutput}
            }
        }
    } <-
    asks FileScheme.account
  shellLogsFilepath <- getShellLogsFilepath accountDirectory
  liftIO $
    Snarkjs.generateProof
      Snarkjs.ProveScheme
        { provingKey = toFilePath (accountDirectory </> provingKey)
        , witness = toFilePath (sessionDirectory </> witnessOutput)
        , proofOutput = toFilePath (sessionDirectory </> proofOutput)
        , statementOutput = toFilePath (sessionDirectory </> statementOutput)
        }
      &!> StdOut
      &> Append shellLogsFilepath
  maybeProofValue <- readFrom (sessionDirectory </> proofOutput)
  pure $ case maybeProofValue of
    Nothing -> Proof Null
    Just value -> Proof value
