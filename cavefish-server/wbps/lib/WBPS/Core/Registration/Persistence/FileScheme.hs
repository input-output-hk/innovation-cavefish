-- | This module provides functionality to derive directory paths for user accounts
-- based on their wallet public keys within a file scheme structure.
module WBPS.Core.Registration.Persistence.FileScheme (
  deriveAccountDirectoryFrom,
  -- | Derive the directory path for a user account
  getAccountDirectoryName,
  -- | Get the directory name for a given account ID
) where

import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.RWS (MonadIO, MonadReader (ask))
import Path (parseRelDir, (</>))
import WBPS.Core.Failure (
  WBPSFailure (RegistrationIdInvalid),
 )
import WBPS.Core.Registration.Persistence.FileScheme.Directories qualified as Directory
import WBPS.Core.Registration.RegistrationId (RegistrationId)
import WBPS.Core.Setup.Circuit.FileScheme (FileScheme (FileScheme, accounts))

-- | Derive the directory path for a user account based on their wallet public key.
-- This function constructs the directory path by combining the base accounts directory
-- from the file scheme with the specific account directory name derived from the user's
-- wallet public key.
deriveAccountDirectoryFrom ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  RegistrationId -> m Directory.Account
deriveAccountDirectoryFrom registrationId = do
  FileScheme {accounts} <- ask
  (accounts </>) <$> getAccountDirectoryName registrationId

-- | Get the directory name for a given account ID.
-- This function attempts to parse the account ID into a valid directory name.
getAccountDirectoryName :: MonadError [WBPSFailure] m => RegistrationId -> m Directory.AccountName
getAccountDirectoryName registrationId =
  either
    (const . throwError $ [RegistrationIdInvalid registrationId])
    pure
    (Path.parseRelDir (show registrationId))
