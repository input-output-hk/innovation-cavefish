module WBPS.Core.Session.Witness (
  generateWitness,
  prepareInputs,
) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (def)
import Data.Text (Text)
import Data.Word (Word8)
import GHC.Generics (Generic)
import Path (File, Path, toFilePath, (</>))
import Shh (Stream (Append, StdOut), (&!>), (&>))
import WBPS.Adapter.Data.Aeson (jsonNumberToText)
import WBPS.Adapter.Math.AffinePoint qualified as AffinePoint
import WBPS.Adapter.Math.Integer qualified as Integer
import WBPS.Adapter.Path (writeTo)
import WBPS.Core.Failure (RegistrationFailed)
import WBPS.Core.FileScheme (
  Account (Account, session),
  FileScheme (FileScheme, account, setup),
  Session (Session, witness),
  Setup (Setup, witness),
  WitnessGeneration (WitnessGeneration, input, output),
  WitnessGenerationSetup (WitnessGenerationSetup, wasm),
  getShellLogsFilepath,
 )
import WBPS.Core.Groth16.Setup qualified as Groth16
import WBPS.Core.Keys.Ed25519 qualified as Ed25519
import WBPS.Core.Keys.ElGamal qualified as ElGamal
import WBPS.Core.Primitives.Snarkjs qualified as Snarkjs
import WBPS.Core.Registration.Account (AccountCreated (AccountCreated, setup, userWalletPublicKey))
import WBPS.Core.Registration.FileScheme (deriveAccountDirectoryFrom)
import WBPS.Core.Session.Challenge (Challenge)
import WBPS.Core.Session.Challenge qualified as Challenge
import WBPS.Core.Session.Commitment (Commitment (Commitment, id, payload), CommitmentPayload (CommitmentPayload))
import WBPS.Core.Session.Commitment.Scalars (CommitmentScalars (CommitmentScalars, ekPowRho))
import WBPS.Core.Session.FileScheme (deriveExistingSessionDirectoryFrom)
import WBPS.Core.Session.R (R)
import WBPS.Core.Session.Session (CommitmentDemonstrated (CommitmentDemonstrated, commitment, commitmentScalars, message, publicMessage, rho))
import WBPS.Core.ZK.Message (MessageBits (MessageBits), messageBitsToWord8s, messageToBits, publicMessageToMessageBits)

data CircuitInputs = CircuitInputs
  { signer_key :: [Word8]
  , solver_encryption_key :: [Text]
  , commitment_point_bits :: [Word8]
  , commitment_point_affine :: [Text]
  , commitment_randomizer_rho :: Text
  , commitment_payload :: [Text]
  , challenge :: [Word8]
  , message_public_part :: [Word8]
  , message_private_part :: [Word8]
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

generateWitness ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  AccountCreated ->
  CommitmentDemonstrated ->
  R ->
  Challenge ->
  m ()
generateWitness
  accountCreated@AccountCreated {userWalletPublicKey}
  commitmentDemonstrated@CommitmentDemonstrated {commitment = WBPS.Core.Session.Commitment.Commitment {id = commitmentId}}
  bigR
  challengeValue = do
    sessionDirectory <- deriveExistingSessionDirectoryFrom userWalletPublicKey commitmentId
    accountDirectory <- deriveAccountDirectoryFrom userWalletPublicKey
    FileScheme
      { setup = Setup {witness = WitnessGenerationSetup {wasm}}
      , account = Account {session = Session {witness = WitnessGeneration {input, output}}}
      } <-
      ask
    saveCircuitInputs
      (sessionDirectory </> input)
      (prepareInputs accountCreated commitmentDemonstrated bigR challengeValue)
    shellLogsFilepath <- getShellLogsFilepath accountDirectory
    liftIO $
      Snarkjs.generateWitness
        Snarkjs.WitnessScheme
          { wasm = toFilePath wasm
          , input = toFilePath (sessionDirectory </> input)
          , witnessOutput = toFilePath (sessionDirectory </> output)
          }
        &!> StdOut
        &> Append shellLogsFilepath

prepareInputs ::
  AccountCreated ->
  CommitmentDemonstrated ->
  R ->
  Challenge ->
  CircuitInputs
prepareInputs
  AccountCreated
    { userWalletPublicKey
    , setup = Groth16.Setup {encryptionKeys = ElGamal.KeyPair {ek = ElGamal.EncryptionKey solverKeyPoint}}
    }
  CommitmentDemonstrated
    { message
    , publicMessage
    , rho
    , commitmentScalars = CommitmentScalars {ekPowRho = commitmentPoint}
    , commitment = Commitment {payload = CommitmentPayload (MessageBits payloadBits)}
    }
  _bigR
  challengeValue =
    CircuitInputs
      { signer_key = Ed25519.userWalletPublicKeyToWord8s userWalletPublicKey
      , solver_encryption_key = AffinePoint.toText solverKeyPoint
      , commitment_point_bits = AffinePoint.toBits commitmentPoint
      , commitment_point_affine = AffinePoint.toText commitmentPoint
      , commitment_randomizer_rho = jsonNumberToText rho
      , commitment_payload = map Integer.toText payloadBits
      , challenge = Challenge.toWord8s challengeValue
      , message_public_part = messageBitsToWord8s (publicMessageToMessageBits publicMessage)
      , message_private_part = messageBitsToWord8s (messageToBits def message)
      }

saveCircuitInputs :: MonadIO m => Path b File -> CircuitInputs -> m ()
saveCircuitInputs = writeTo
