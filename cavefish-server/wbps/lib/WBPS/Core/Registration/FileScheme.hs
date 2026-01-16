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
  WBPSFailure (AccountIdInvalidToCreateAFolder),
 )
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Registration.FileScheme.Directories qualified as Directory
import WBPS.Core.Registration.Registered (AccountId (AccountId), deriveId)
import WBPS.Core.Setup.Circuit.FileScheme (FileScheme (FileScheme, accounts))

-- | Derive the directory path for a user account based on their wallet public key.
-- This function constructs the directory path by combining the base accounts directory
-- from the file scheme with the specific account directory name derived from the user's
-- wallet public key.
deriveAccountDirectoryFrom ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  UserWalletPublicKey -> m Directory.Account
deriveAccountDirectoryFrom userWalletPublicKey = do
  FileScheme {accounts} <- ask
  (accounts </>) <$> (getAccountDirectoryName . deriveId $ userWalletPublicKey)

-- | Get the directory name for a given account ID.
-- This function attempts to parse the account ID into a valid directory name.
getAccountDirectoryName :: MonadError [WBPSFailure] m => AccountId -> m Directory.AccountName
getAccountDirectoryName (AccountId x) =
  either
    (const . throwError $ [AccountIdInvalidToCreateAFolder x])
    pure
    (Path.parseRelDir x)
