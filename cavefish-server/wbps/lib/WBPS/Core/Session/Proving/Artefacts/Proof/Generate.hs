{-# LANGUAGE QuasiQuotes #-}

module WBPS.Core.Session.Proving.Artefacts.Proof.Generate (
  generateProof,
) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader.Class (asks)
import Path (reldir, toFilePath, (</>))
import Shh (Stream (Append, StdOut), (&!>), (&>))
import WBPS.Adapter.Monad.Control (whenNothingThrow)
import WBPS.Adapter.Path (readFrom)
import WBPS.Core.Failure (WBPSFailure (SessionProofNotFound))
import WBPS.Core.Primitives.Snarkjs qualified as Snarkjs
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Registration.FileScheme (deriveAccountDirectoryFrom)
import WBPS.Core.Session.Demonstration.Artefacts.Commitment (CommitmentId)
import WBPS.Core.Session.FileScheme (deriveExistingSessionDirectoryFrom)
import WBPS.Core.Session.Proving.Artefacts.Proof (Proof (Proof))
import WBPS.Core.Session.Session (deriveId, unSessionId)
import WBPS.Core.Setup.Circuit.FileScheme (
  Account (Account, registration, session),
  FileScheme,
  ProofGeneration (ProofGeneration, proof, statement),
  Proving (Proving, proof, witness),
  Registration (Registration, provingKey),
  Session (Session, proving),
  WitnessGeneration (WitnessGeneration, output),
  getShellLogsFilepath,
 )
import WBPS.Core.Setup.Circuit.FileScheme qualified as FileScheme

generateProof ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
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
  let provedDirectory = sessionDirectory </> [reldir|proved|]
  let provedWitnessDirectory = provedDirectory </> [reldir|witness|]
  liftIO $
    Snarkjs.generateProof
      Snarkjs.ProveScheme
        { provingKey = toFilePath (accountDirectory </> [reldir|registered|] </> provingKey)
        , witness = toFilePath (provedWitnessDirectory </> witnessOutput)
        , proofOutput = toFilePath (provedDirectory </> proofOutput)
        , statementOutput = toFilePath (provedDirectory </> statementOutput)
        }
      &!> StdOut
      &> Append shellLogsFilepath
  Proof
    <$> ( readFrom (provedDirectory </> proofOutput)
            >>= whenNothingThrow [SessionProofNotFound (show userWalletPublicKey) (unSessionId . deriveId $ commitmentId)]
        )
