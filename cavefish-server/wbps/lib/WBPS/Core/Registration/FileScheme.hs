module WBPS.Core.Registration.FileScheme (
  deriveDirectoryAccountFrom,
  getAccountDirectoryName,
) where

import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.RWS (MonadIO, MonadReader (ask))
import Path (parseRelDir, (</>))
import WBPS.Core.Failure (
  RegistrationFailed (AccountIdInvalidToCreateAFolder),
 )
import WBPS.Core.FileScheme (FileScheme (..))
import WBPS.Core.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Registration.Account (AccountId (..), accountId)
import WBPS.Core.Registration.FileScheme.Directories qualified as Directory

deriveDirectoryAccountFrom ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  UserWalletPublicKey -> m Directory.Account
deriveDirectoryAccountFrom userWalletPublicKey = do
  FileScheme {accounts} <- ask
  (accounts </>) <$> (getAccountDirectoryName . accountId $ userWalletPublicKey)

getAccountDirectoryName :: MonadError [RegistrationFailed] m => AccountId -> m Directory.AccountName
getAccountDirectoryName account@(AccountId x) =
  either
    (const . throwError $ [AccountIdInvalidToCreateAFolder account])
    pure
    (Path.parseRelDir x)
