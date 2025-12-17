module WBPS.Core.Registration.FetchAccounts (
  loadAccount,
  loadExistingAccount,
  loadAccounts,
) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import Data.String (IsString (fromString))
import Path (Dir, Path, toFilePath, (</>))
import Path.IO (doesDirExist, listDirRel)
import WBPS.Adapter.Monad.Control (ifM, whenNothingThrow)
import WBPS.Adapter.Path (readFrom)
import WBPS.Core.Failure (RegistrationFailed (..))
import WBPS.Core.FileScheme
import WBPS.Core.Keys.Ed25519 (UserWalletPublicKey (UserWalletPublicKey))
import WBPS.Core.Registration.Account
import WBPS.Core.Registration.FileScheme
import WBPS.Core.Registration.PublicVerificationContext (PublicVerificationContext (..))

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
  traverse loadExistingAccount recordedKeys

loadAccount ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  UserWalletPublicKey -> m (Maybe AccountCreated)
loadAccount userWalletPublicKey = do
  account <- deriveDirectoryAccountFrom userWalletPublicKey
  ifM
    (not <$> doesDirExist account)
    (return Nothing)
    (Just <$> loadExistingAccount userWalletPublicKey)

loadExistingAccount ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  UserWalletPublicKey -> m AccountCreated
loadExistingAccount userWalletPublicKey = do
  account <- deriveDirectoryAccountFrom userWalletPublicKey
  FileScheme {..} <- ask
  AccountCreated
    userWalletPublicKey
    (account </> provingKey)
    <$> (readFrom (account </> encryptionKeys) >>= whenNothingThrow [EncryptionKeysNotFound userWalletPublicKey])
    <*> ( PublicVerificationContext (account </> verificationContext)
            <$> (readFrom (account </> verificationContext) >>= whenNothingThrow [EncryptionKeysNotFound userWalletPublicKey])
        )
