{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RankNTypes #-}

module WBPS (
  register,
  withFileSchemeIO,
  SignerKey,
  getVerificationContext,
  RegistrationFailure (..),
) where

import Cardano.Crypto.DSIGN.Class (
  SignKeyDSIGN,
  VerKeyDSIGN,
  deriveVerKeyDSIGN,
  genKeyDSIGN,
  seedSizeDSIGN,
 )
import Cardano.Crypto.DSIGN.Ed25519 (Ed25519DSIGN)
import Control.Exception (SomeException)
import Control.Monad (unless, when)
import Control.Monad.Error.Class
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class
import Control.Monad.RWS (MonadReader, asks)
import Control.Monad.Reader (ask, runReaderT)
import Control.Monad.Trans.Maybe
import Data.Aeson (Value, eitherDecode)
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Functor
import Data.Validation (Validation (..))
import GHC.Base (when)
import Path
import Path.IO
import Shh hiding (Failure)
import WBPS.Adapter.CardanoCryptoClass.Crypto
import WBPS.Adapter.Data
import WBPS.Core (SignerKey)
import WBPS.Core.FileScheme
import WBPS.Core.Primitives.Snarkjs
import WBPS.Core.Primitives.Snarkjs qualified as Snarkjs
import WBPS.Core.Primitives.SnarkjsOverFileScheme as SnarkJs

register ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailure] m) =>
  SignerKey ->
  m ()
register signerKey = do
  scheme@FileScheme {..} <- ask
  accountName <- getAccountFileName . accountId $ signerKey
  let account = accounts </> accountName
  ensureDir account
  let verificationContextPath = account </> verificationContext
  verificationExists <- liftIO (doesFileExist verificationContextPath)
  unless verificationExists $ do
    generateProvingKeyProcess <- getGenerateProvingKeyProcess account
    generateVerificationKeyProcess <- getGenerateVerificationKeyProcess account
    shellLogsFilepath <- getShellLogsFilepath account
    liftIO $
      (generateProvingKeyProcess >> generateVerificationKeyProcess)
        &!> StdOut
        &> Append shellLogsFilepath

getAccountFileName :: MonadError [RegistrationFailure] m => AccountId -> m AccountName
getAccountFileName account@(AccountId x) =
  either
    (const . throwError $ [AccountIdInvalidToCreateAFolder account])
    pure
    (Path.parseRelDir x)

withFileSchemeIO ::
  FileScheme ->
  (forall m. (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailure] m) => m a) ->
  IO (Either [RegistrationFailure] a)
withFileSchemeIO scheme action =
  runExceptT $ runReaderT action scheme

newtype AccountId = AccountId String deriving (Show, Eq)

data RegistrationFailure
  = AccountIdInvalidToCreateAFolder AccountId
  | VerificationContextMissing Account
  | VerificationContextInvalidJSON Account String
  deriving (Show, Eq)

accountId :: SignerKey -> AccountId
accountId = AccountId . show

getVerificationContext ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailure] m) =>
  SignerKey ->
  m Value
getVerificationContext signerKey = do
  FileScheme {accounts, verificationContext = verificationContextRel} <- ask
  accountDir <- accountDirectory accounts signerKey
  register signerKey
  loadVerificationContext accountDir verificationContextRel

accountDirectory ::
  MonadError [RegistrationFailure] m =>
  Accounts ->
  SignerKey ->
  m Account
accountDirectory accountsDir signer = do
  name <- getAccountFileName (accountId signer)
  pure (accountsDir </> name)

loadVerificationContext ::
  (MonadIO m, MonadError [RegistrationFailure] m) =>
  Account ->
  Path Rel File ->
  m Value
loadVerificationContext accountDir verificationContextRel = do
  let verificationContextPath = accountDir </> verificationContextRel
  exists <- liftIO (doesFileExist verificationContextPath)
  unless exists $
    throwError [VerificationContextMissing accountDir]
  bytes <- liftIO (BL.readFile (Path.toFilePath verificationContextPath))
  case eitherDecode bytes of
    Left err -> throwError [VerificationContextInvalidJSON accountDir err]
    Right value -> pure value

-- Below is Experimental (under progress)

data Message = Message {private :: ByteString, public :: ByteString}

data AccountKeys = AccountKeys
  { provingKey :: Path Abs File
  , verificationContext :: Path Abs File
  }

type InstanceId = String

addInstance :: (MonadIO m, MonadReader FileScheme m) => AccountId -> Message -> m InstanceId
addInstance accountId message =
  -- generate el Gamal ephemeral key
  -- build commitment
  -- build challenge
  -- create input.json file
  -- call generateWitness
  -- call generateProof
  pure ""

verifyInstance :: (MonadIO m, MonadReader FileScheme m) => AccountId -> InstanceId -> m ()
verifyInstance accountId instanceId =
  -- SnarkJS.verifyProof
  pure ()

data Point = Point {x :: Int, y :: Int}

data Commitment
  = Commitment
  { point :: Point
  , rho :: Int
  , payload :: ByteString
  }

type Challenge = ByteString

data WitnessInput
  = WitnessInput
  { signer_key :: ByteString
  , elGamal_ephemeral_encryption_key :: (ByteString, ByteString)
  , commitment :: Commitment
  }
