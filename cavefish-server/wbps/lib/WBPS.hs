{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module WBPS (
  register,
  withFileSchemeIO,
  loadAccount,
  loadAccounts,
  AccountCreated (..),
  RegistrationFailed (..),
  PublicVerificationContext (..),
) where

import Cardano.Crypto.DSIGN.Class (
  SignKeyDSIGN,
  VerKeyDSIGN,
  deriveVerKeyDSIGN,
  genKeyDSIGN,
  seedSizeDSIGN,
 )
import Cardano.Crypto.DSIGN.Ed25519 (Ed25519DSIGN)
import Control.Exception (SomeException, displayException, try)
import Control.Monad (filterM, unless, when)
import Control.Monad.Error.Class
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class
import Control.Monad.RWS (MonadReader, asks)
import Control.Monad.Reader (ask, runReaderT)
import Control.Monad.Trans.Maybe
import Crypto.Random (MonadRandom)
import Data.Aeson (Value, eitherDecode)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key (toString)
import Data.Bool (bool)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Functor
import Data.Maybe (mapMaybe)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Validation (Validation (..))
import GHC.Base (when)
import GHC.Generics
import Path
import Path.IO
import Shh hiding (Failure, toFilePath)
import WBPS.Adapter.CardanoCryptoClass.Crypto hiding (PublicKey)
import WBPS.Adapter.Data
import WBPS.Adapter.Monad.Control
import WBPS.Adapter.Path
import WBPS.Core.FileScheme
import WBPS.Core.Keys.Ed25519 (PublicKey (..), UserWalletPublicKey (..))
import WBPS.Core.Keys.ElGamal qualified as ElGamal
import WBPS.Core.Primitives.Snarkjs
import WBPS.Core.Primitives.Snarkjs qualified as Snarkjs
import WBPS.Core.Primitives.SnarkjsOverFileScheme as SnarkJs

data PublicVerificationContext = PublicVerificationContext {filePath :: Path Abs File, asJson :: Value}
  deriving (Eq, Show, Ord, Generic)

data AccountCreated
  = AccountCreated
  { userWalletPublicKey :: UserWalletPublicKey
  , provingKey :: Path Abs File
  , encryptionKeys :: ElGamal.KeyPair
  , publicVerificationContext :: PublicVerificationContext
  }
  deriving (Ord, Eq, Show)

getRecordedUserWalletPublicKeys :: MonadIO m => Path b Dir -> m [UserWalletPublicKey]
getRecordedUserWalletPublicKeys p = do
  a <- fst <$> (liftIO . listDirRel $ p)
  return $ fromString . takeWhile (/= '/') . toFilePath <$> a

loadAccounts ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  m [AccountCreated]
loadAccounts = do
  FileScheme {..} <- ask
  recordedKeys <- getRecordedUserWalletPublicKeys accounts
  traverse loadAccount' recordedKeys

loadAccount ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  UserWalletPublicKey -> m (Maybe AccountCreated)
loadAccount userWalletPublicKey = do
  account <- deriveAccountFrom userWalletPublicKey
  ifM
    (not <$> doesDirExist account)
    (return Nothing)
    (Just <$> loadAccount' userWalletPublicKey)

loadAccount' ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  UserWalletPublicKey -> m AccountCreated
loadAccount' userWalletPublicKey = do
  account <- deriveAccountFrom userWalletPublicKey
  FileScheme {..} <- ask
  AccountCreated
    userWalletPublicKey
    (account </> provingKey)
    <$> ( readFrom (account </> encryptionKeys)
            >>= whenNothingThrow [EncryptionKeysNotFound userWalletPublicKey]
        )
    <*> ( PublicVerificationContext (account </> verificationContext)
            <$> ( readFrom (account </> verificationContext)
                    >>= whenNothingThrow [EncryptionKeysNotFound userWalletPublicKey]
                )
        )

register ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  UserWalletPublicKey ->
  m AccountCreated
register userWalletPublicKey =
  loadAccount userWalletPublicKey
    >>= \case
      Just AccountCreated {..} -> throwError [AccountAlreadyRegistered userWalletPublicKey]
      Nothing -> do
        register' =<< deriveAccountFrom userWalletPublicKey
        loadAccount' userWalletPublicKey

deriveAccountFrom ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  UserWalletPublicKey -> m Account
deriveAccountFrom userWalletPublicKey = do
  FileScheme {..} <- ask
  (accounts </>) <$> (getAccountFileName . accountId $ userWalletPublicKey)

register' ::
  (MonadIO m, MonadReader FileScheme m) =>
  Account ->
  m ()
register' account = do
  FileScheme {..} <- ask
  ensureDir account
  ElGamal.generateKeypair >>= writeTo (account </> encryptionKeys)
  generateProvingKeyProcess <- getGenerateProvingKeyProcess account
  generateVerificationKeyProcess <- getGenerateVerificationKeyProcess account
  shellLogsFilepath <- getShellLogsFilepath account
  liftIO $
    (generateProvingKeyProcess >> generateVerificationKeyProcess)
      &!> StdOut
      &> Append shellLogsFilepath

getAccountFileName :: MonadError [RegistrationFailed] m => AccountId -> m AccountName
getAccountFileName account@(AccountId x) =
  either
    (const . throwError $ [AccountIdInvalidToCreateAFolder account])
    pure
    (Path.parseRelDir x)

withFileSchemeIO ::
  FileScheme ->
  (forall m. (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) => m a) ->
  IO (Either [RegistrationFailed] a)
withFileSchemeIO scheme action =
  runExceptT $ runReaderT action scheme

newtype AccountId = AccountId String deriving (Show, Eq)

data RegistrationFailed
  = AccountIdInvalidToCreateAFolder AccountId
  | VerificationNotFound UserWalletPublicKey
  | EncryptionKeysNotFound UserWalletPublicKey
  | AccountAlreadyRegistered UserWalletPublicKey
  deriving (Show, Eq)

accountId :: UserWalletPublicKey -> AccountId
accountId (UserWalletPublicKey (PublicKey x)) = AccountId . show $ x
