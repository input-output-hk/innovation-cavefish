{-# LANGUAGE QuasiQuotes #-}

module WBPS.Core.Session.Steps.Proving.Artefacts.Witness (
  generate,
  prepareInputs,
  saveCircuitInputs,
) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Path (File, Path, reldir, toFilePath, (</>))
import Shh (Stream (Append, StdOut), (&!>), (&>))
import WBPS.Adapter.CLI.Wrapped.Snarkjs qualified as Snarkjs
import WBPS.Adapter.Math.AffinePoint qualified as AffinePoint
import WBPS.Adapter.Path (writeTo)
import WBPS.Core.Failure (WBPSFailure)
import WBPS.Core.Registration.Artefacts.Groth16.Setup qualified as Groth16
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 qualified as Ed25519
import WBPS.Core.Registration.Artefacts.Keys.ElGamal qualified as ElGamal
import WBPS.Core.Registration.Persistence.FileScheme (deriveAccountDirectoryFrom)
import WBPS.Core.Registration.Registered (Registered (Registered, registrationId, setup))
import WBPS.Core.Registration.RegistrationId (RegistrationId (RegistrationId, userWalletPublicKey))
import WBPS.Core.Session.Persistence.FileScheme (deriveExistingSessionDirectoryFrom)
import WBPS.Core.Session.SessionId (SessionId (SessionId, commitmentId, registrationId))
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Commitment (
  Commitment (Commitment, id, payload),
  CommitmentPayload,
 )
import WBPS.Core.Session.Steps.Demonstration.Artefacts.PreparedMessage (
  CircuitMessage (CircuitMessage, private, public),
  MessageBits,
  PreparedMessage (PreparedMessage, circuit),
 )
import WBPS.Core.Session.Steps.Demonstration.Artefacts.R (R)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.R qualified as R
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Rho (Rho)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Scalars (Scalars (Scalars, ekPowRho, rho))
import WBPS.Core.Session.Steps.Demonstration.Demonstrated (CommitmentDemonstrated (CommitmentDemonstrated, commitment, preparedMessage, scalars))
import WBPS.Core.Session.Steps.Proving.Artefacts.Challenge (Challenge)
import WBPS.Core.Setup.Circuit.FileScheme (
  Account (Account, session),
  FileScheme (FileScheme, account, setup),
  Proving (Proving, bigR, challenge, witness),
  Session (Session, proving),
  Setup (Setup, witness),
  WitnessGeneration (WitnessGeneration, input, output),
  WitnessGenerationSetup (WitnessGenerationSetup, wasm),
  getShellLogsFilepath,
 )

data CircuitInputs = CircuitInputs
  { signer_key :: [Word8]
  , solver_encryption_key :: [Text]
  , solver_encryption_key_pow_rho :: [Text]
  , commitment_point_bits :: [Word8]
  , commitment_randomizer_rho :: Rho
  , commitment_payload :: CommitmentPayload
  , challenge :: Challenge
  , message_public_part :: MessageBits
  , message_private_part :: MessageBits
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

generate ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  Registered ->
  CommitmentDemonstrated ->
  R ->
  Challenge ->
  m ()
generate
  registered@Registered {registrationId}
  commitmentDemonstrated@CommitmentDemonstrated {commitment = Commitment {id = commitmentId}}
  bigR
  challenge = do
    let sessionId = SessionId {..}
    sessionDirectory <- deriveExistingSessionDirectoryFrom sessionId
    accountDirectory <- deriveAccountDirectoryFrom registrationId
    FileScheme
      { setup = Setup {witness = WitnessGenerationSetup {wasm}}
      , account =
        Account
          { session =
            Session
              { proving =
                Proving {bigR = bigRFile, challenge = challengeFile, witness = WitnessGeneration {input, output}}
              }
          }
      } <-
      ask

    writeTo (sessionDirectory </> [reldir|proved|] </> bigRFile) bigR
    writeTo (sessionDirectory </> [reldir|proved|] </> challengeFile) challenge

    saveCircuitInputs
      (sessionDirectory </> [reldir|proved|] </> [reldir|witness|] </> input)
      (prepareInputs registered commitmentDemonstrated bigR challenge)

    shellLogsFilepath <- getShellLogsFilepath accountDirectory
    liftIO $
      Snarkjs.generateWitness
        Snarkjs.WitnessScheme
          { wasm = toFilePath wasm
          , input = toFilePath (sessionDirectory </> [reldir|proved|] </> [reldir|witness|] </> input)
          , witnessOutput = toFilePath (sessionDirectory </> [reldir|proved|] </> [reldir|witness|] </> output)
          }
        &!> StdOut
        &> Append shellLogsFilepath

prepareInputs ::
  Registered ->
  CommitmentDemonstrated ->
  R ->
  Challenge ->
  CircuitInputs
prepareInputs
  Registered
    { registrationId = RegistrationId {userWalletPublicKey}
    , setup = Groth16.Setup {encryptionKeys = ElGamal.KeyPair {ek = ElGamal.EncryptionKey solverKeyPoint}}
    }
  CommitmentDemonstrated
    { preparedMessage = PreparedMessage {circuit = CircuitMessage {public, private}}
    , scalars = Scalars {rho, ekPowRho}
    , commitment = Commitment {payload}
    }
  bigR
  challengeValue =
    CircuitInputs
      { signer_key = Ed25519.userWalletPublicKeyToWord8s userWalletPublicKey
      , solver_encryption_key = AffinePoint.toText solverKeyPoint
      , solver_encryption_key_pow_rho = AffinePoint.toText ekPowRho
      , commitment_point_bits = R.toWord8s bigR
      , commitment_randomizer_rho = rho
      , commitment_payload = payload
      , challenge = challengeValue
      , message_public_part = public
      , message_private_part = private
      }

saveCircuitInputs :: MonadIO m => Path b File -> CircuitInputs -> m ()
saveCircuitInputs = writeTo
