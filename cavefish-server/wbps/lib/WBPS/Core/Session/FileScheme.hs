{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RankNTypes #-}

module WBPS.Core.Session.FileScheme (
  deriveSessionDirectoryFrom,
  deriveExistingSessionDirectoryFrom,
  getSessionDirectoryName,
) where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader.Class (asks)
import Path (parseRelDir, (</>))
import Path.IO (doesDirExist)
import WBPS.Adapter.Monad.Control (ifM)
import WBPS.Core.Failure (
  RegistrationFailed (SessionIdInvalidToCreateAFolder, SessionNotFound),
 )
import WBPS.Core.FileScheme (FileScheme)
import WBPS.Core.FileScheme qualified as FileScheme
import WBPS.Core.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Registration.FileScheme (deriveAccountDirectoryFrom)
import WBPS.Core.Session.Commitment (CommitmentId)
import WBPS.Core.Session.FileScheme.Directories qualified as Directory
import WBPS.Core.Session.Session (
  SessionId (SessionId),
  deriveId,
 )

deriveExistingSessionDirectoryFrom ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  UserWalletPublicKey -> CommitmentId -> m Directory.Session
deriveExistingSessionDirectoryFrom userWalletPublicKey commitmentId = do
  sessionDirectory <- deriveSessionDirectoryFrom userWalletPublicKey commitmentId
  ifM
    (not <$> doesDirExist sessionDirectory)
    (throwError [SessionNotFound userWalletPublicKey commitmentId])
    (return sessionDirectory)

deriveSessionDirectoryFrom ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  UserWalletPublicKey -> CommitmentId -> m Directory.Session
deriveSessionDirectoryFrom userWalletPublicKey commitmentId = do
  accountDirectory <- deriveAccountDirectoryFrom userWalletPublicKey
  FileScheme.Account {sessions} <- asks FileScheme.account
  ((accountDirectory </> sessions) </>) <$> (getSessionDirectoryName . deriveId $ commitmentId)

getSessionDirectoryName :: MonadError [RegistrationFailed] m => SessionId -> m Directory.SessionName
getSessionDirectoryName sessionId@(SessionId x) =
  either
    (const . throwError $ [SessionIdInvalidToCreateAFolder sessionId])
    pure
    (Path.parseRelDir x)
