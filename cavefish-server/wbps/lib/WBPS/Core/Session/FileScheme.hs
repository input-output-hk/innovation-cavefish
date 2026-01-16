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
  WBPSFailure (SessionIdInvalidToCreateAFolder, SessionNotFound),
 )
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Registration.FileScheme (deriveAccountDirectoryFrom)
import WBPS.Core.Session.Demonstration.Artefacts.Commitment (CommitmentId)
import WBPS.Core.Session.FileScheme.Directories qualified as Directory
import WBPS.Core.Session.Session (
  SessionId (SessionId),
  deriveId,
 )
import WBPS.Core.Setup.Circuit.FileScheme (FileScheme)
import WBPS.Core.Setup.Circuit.FileScheme qualified as FileScheme

deriveExistingSessionDirectoryFrom ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  UserWalletPublicKey -> CommitmentId -> m Directory.Session
deriveExistingSessionDirectoryFrom userWalletPublicKey commitmentId = do
  sessionDirectory <- deriveSessionDirectoryFrom userWalletPublicKey commitmentId
  ifM
    (not <$> doesDirExist sessionDirectory)
    (throwError [SessionNotFound (show userWalletPublicKey) (commitmentIdToString commitmentId)])
    (return sessionDirectory)

deriveSessionDirectoryFrom ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  UserWalletPublicKey -> CommitmentId -> m Directory.Session
deriveSessionDirectoryFrom userWalletPublicKey commitmentId = do
  accountDirectory <- deriveAccountDirectoryFrom userWalletPublicKey
  FileScheme.Account {sessions} <- asks FileScheme.account
  ((accountDirectory </> sessions) </>) <$> (getSessionDirectoryName . deriveId $ commitmentId)

getSessionDirectoryName :: MonadError [WBPSFailure] m => SessionId -> m Directory.SessionName
getSessionDirectoryName (SessionId x) =
  either
    (const . throwError $ [SessionIdInvalidToCreateAFolder x])
    pure
    (Path.parseRelDir x)

commitmentIdToString :: CommitmentId -> String
commitmentIdToString commitmentId =
  let SessionId sessionId = deriveId commitmentId
   in sessionId
