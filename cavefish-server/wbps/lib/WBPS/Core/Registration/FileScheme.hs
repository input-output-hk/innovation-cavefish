-- | This module provides functionality to derive directory paths for user accounts
-- based on their wallet public keys within a file scheme structure.
module WBPS.Core.Registration.FileScheme (
  deriveAccountDirectoryFrom,
  -- | Derive the directory path for a user account
  getAccountDirectoryName,
  -- | Get the directory name for a given account ID
) where

import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.RWS (MonadIO, MonadReader (ask))
import Path (parseRelDir, (</>))
import WBPS.Core.Failure (
  RegistrationFailed (AccountIdInvalidToCreateAFolder),
 )
import WBPS.Core.FileScheme (FileScheme (FileScheme, accounts))
import WBPS.Core.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Registration.Account (AccountId (AccountId), deriveId)
import WBPS.Core.Registration.FileScheme.Directories qualified as Directory

-- | Derive the directory path for a user account based on their wallet public key.
-- This function constructs the directory path by combining the base accounts directory
-- from the file scheme with the specific account directory name derived from the user's
-- wallet public key.
deriveAccountDirectoryFrom ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  UserWalletPublicKey -> m Directory.Account
deriveAccountDirectoryFrom userWalletPublicKey = do
  FileScheme {accounts} <- ask
  (accounts </>) <$> (getAccountDirectoryName . deriveId $ userWalletPublicKey)

-- | Get the directory name for a given account ID.
-- This function attempts to parse the account ID into a valid directory name.
getAccountDirectoryName :: MonadError [RegistrationFailed] m => AccountId -> m Directory.AccountName
getAccountDirectoryName account@(AccountId x) =
  either
    (const . throwError $ [AccountIdInvalidToCreateAFolder account])
    pure
    (Path.parseRelDir x)
