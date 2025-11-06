{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RankNTypes #-}

module WBPS (register, withFileSchemeIO, SignerKey) where

import Cardano.Crypto.DSIGN.Class (
  SignKeyDSIGN,
  VerKeyDSIGN,
  deriveVerKeyDSIGN,
  genKeyDSIGN,
  seedSizeDSIGN,
 )
import Cardano.Crypto.DSIGN.Ed25519 (Ed25519DSIGN)
import Control.Exception (SomeException)
import Control.Monad ()
import Control.Monad.Error.Class
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class
import Control.Monad.RWS (MonadReader, asks)
import Control.Monad.Reader (ask, runReaderT)
import Control.Monad.Trans.Maybe
import Data.Bool (bool)
import Data.ByteString (ByteString)
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
  SignerKey -> m ()
register signerKey = do
  scheme@FileScheme {..} <- ask
  ensureDir accounts
  accountName <- getAccountFileName . accountId $ signerKey
  account <- createAccountDirectory $ accounts </> accountName
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
  = AccountExistAlready Account
  | AccountIdInvalidToCreateAFolder AccountId
  deriving (Show, Eq)

accountId :: SignerKey -> AccountId
accountId = AccountId . show

createAccountDirectory ::
  (MonadIO m, MonadError [RegistrationFailure] m) =>
  Account ->
  m Account
createAccountDirectory account = do
  whenM (liftIO . doesDirExist $ account) $ throwError [AccountExistAlready account]
  liftIO . ensureDir $ account
  pure account

-- Below is Experimental (under progress)

data Message = Message {private :: ByteString, public :: ByteString}

data AccountKeys = AccountKeys
  { provingKey :: Path Abs File
  , verificationKey :: Path Abs File
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
