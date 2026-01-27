{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RankNTypes #-}

module WBPS.Core.Session.Persistence.FileScheme (
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
  WBPSFailure (SessionIdInvalidToCreateAFolder, SessionNotFound),
 )
import WBPS.Core.Registration.Persistence.FileScheme (deriveAccountDirectoryFrom)
import WBPS.Core.Session.Persistence.FileScheme.Directories qualified as Directory
import WBPS.Core.Session.SessionId (
  SessionId (SessionId, registrationId),
  toString,
 )
import WBPS.Core.Setup.Circuit.FileScheme (FileScheme)
import WBPS.Core.Setup.Circuit.FileScheme qualified as FileScheme

deriveExistingSessionDirectoryFrom ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  SessionId -> m Directory.Session
deriveExistingSessionDirectoryFrom sessionId = do
  sessionDirectory <- deriveSessionDirectoryFrom sessionId
  ifM
    (not <$> doesDirExist sessionDirectory)
    (throwError [SessionNotFound sessionId])
    (return sessionDirectory)

deriveSessionDirectoryFrom ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  SessionId -> m Directory.Session
deriveSessionDirectoryFrom sessionId@SessionId {registrationId} = do
  accountDirectory <- deriveAccountDirectoryFrom registrationId
  FileScheme.Account {sessions} <- asks FileScheme.account
  ((accountDirectory </> sessions) </>) <$> getSessionDirectoryName sessionId

getSessionDirectoryName :: MonadError [WBPSFailure] m => SessionId -> m Directory.SessionName
getSessionDirectoryName sessionId =
  let sessionDirectoryName = toString sessionId
   in either
        (const . throwError $ [SessionIdInvalidToCreateAFolder sessionId])
        pure
        (Path.parseRelDir sessionDirectoryName)
