module WBPS.Core.Session.Proving.Proof.Generate (
  generateProof,
) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader.Class (asks)
import Data.Aeson (Value (Null))
import Path (toFilePath, (</>))
import Shh (Stream (Append, StdOut), (&!>), (&>))
import WBPS.Adapter.Path (readFrom)
import WBPS.Core.Failure (RegistrationFailed)
import WBPS.Core.FileScheme (
  Account (Account, registration, session),
  FileScheme,
  ProofGeneration (ProofGeneration, proof, statement),
  Proving (Proving, proof, witness),
  Registration (Registration, provingKey),
  Session (Session, proving),
  WitnessGeneration (WitnessGeneration, output),
  getShellLogsFilepath,
 )
import WBPS.Core.FileScheme qualified as FileScheme
import WBPS.Core.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Primitives.Snarkjs qualified as Snarkjs
import WBPS.Core.Registration.FileScheme (deriveAccountDirectoryFrom)
import WBPS.Core.Session.Demonstration.Commitment (CommitmentId)
import WBPS.Core.Session.FileScheme (deriveExistingSessionDirectoryFrom)
import WBPS.Core.Session.Proving.Proof (Proof (Proof))

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
