{-# LANGUAGE QuasiQuotes #-}

-- | Module for fetching and loading user accounts from the file system.
-- This module provides functions to load existing accounts, load a specific account,
-- and retrieve all recorded user wallet public keys. It handles errors related to
-- missing encryption keys and uses a file scheme for directory structure.
module WBPS.Core.Session.FetchSession (
  loadSession,
  loadExistingSession,
  loadSessions,
  loadExistingCommitmentDemonstrationEvents,
  -- | Load an existing session
) where

import Control.Monad (join)
import Control.Monad.Error.Class (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader.Class (asks)
import Data.Functor ((<&>))
import Data.String (fromString)
import Path (Dir, Path, reldir, toFilePath, (</>))
import Path.IO (doesDirExist, listDirRel)
import WBPS.Adapter.Monad.Control (ifM, whenNothingThrow)
import WBPS.Adapter.Path (readFrom)
import WBPS.Core.Failure (WBPSFailure (AccountNotFound, EncryptionKeysNotFound, SessionMessageNotFound))
import WBPS.Core.FileScheme (FileScheme)
import WBPS.Core.FileScheme qualified as FileScheme
import WBPS.Core.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Registration.FetchAccounts (loadAccount, loadAccounts)
import WBPS.Core.Registration.FileScheme (deriveAccountDirectoryFrom)
import WBPS.Core.Registration.Registered (Registered (Registered, userWalletPublicKey))
import WBPS.Core.Session.Demonstration.Commitment (CommitmentId)
import WBPS.Core.Session.Demonstration.Demonstrated (CommitmentDemonstrated (CommitmentDemonstrated))
import WBPS.Core.Session.FileScheme (deriveExistingSessionDirectoryFrom)
import WBPS.Core.Session.Session (
  Session (Demonstrated),
  SessionId (SessionId),
  deriveId,
 )

getRecordedCommitmentIds :: MonadIO m => Path b Dir -> m [CommitmentId]
getRecordedCommitmentIds p = do
  a <- fst <$> (liftIO . listDirRel $ p)
  return $ fromString . takeWhile (/= '/') . toFilePath <$> a

loadSessions ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) => m [Session]
loadSessions = do
  ( loadAccounts
      >>= mapM
        ( \Registered {userWalletPublicKey} -> do
            accountDir <- deriveAccountDirectoryFrom userWalletPublicKey
            FileScheme.Account {sessions} <- asks FileScheme.account
            recordedIds <- getRecordedCommitmentIds (accountDir </> sessions)
            traverse (loadExistingSession userWalletPublicKey) recordedIds
        )
    )
    <&> join

loadSession ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  UserWalletPublicKey -> CommitmentId -> m (Maybe Session)
loadSession userWalletPublicKey commitmentId = do
  account <- deriveAccountDirectoryFrom userWalletPublicKey
  ifM
    (not <$> doesDirExist account)
    (return Nothing)
    (Just <$> loadExistingSession userWalletPublicKey commitmentId)

loadExistingSession ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  UserWalletPublicKey -> CommitmentId -> m Session
loadExistingSession userWalletPublicKey commitmentId = do
  account <- loadAccount userWalletPublicKey >>= whenNothingThrow [AccountNotFound (show userWalletPublicKey)]
  sessionDirectory <- deriveExistingSessionDirectoryFrom userWalletPublicKey commitmentId
  demonstration <- asks (FileScheme.demonstration . FileScheme.session . FileScheme.account)
  Demonstrated account
    <$> ( CommitmentDemonstrated
            <$> ( readFrom (sessionDirectory </> [reldir|demonstrated|] </> FileScheme.preparedMessage demonstration)
                    >>= whenNothingThrow [SessionMessageNotFound (show userWalletPublicKey) (commitmentIdToString commitmentId)]
                )
            <*> ( readFrom (sessionDirectory </> [reldir|demonstrated|] </> FileScheme.scalars demonstration)
                    >>= whenNothingThrow [EncryptionKeysNotFound (show userWalletPublicKey)]
                )
            <*> ( readFrom (sessionDirectory </> [reldir|demonstrated|] </> FileScheme.commitment demonstration)
                    >>= whenNothingThrow [EncryptionKeysNotFound (show userWalletPublicKey)]
                )
        )

loadExistingCommitmentDemonstrationEvents ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  UserWalletPublicKey -> CommitmentId -> m (Registered, CommitmentDemonstrated)
loadExistingCommitmentDemonstrationEvents userWalletPublicKey commitmentId = do
  registered <- loadAccount userWalletPublicKey >>= whenNothingThrow [AccountNotFound (show userWalletPublicKey)]
  sessionDirectory <- deriveExistingSessionDirectoryFrom userWalletPublicKey commitmentId
  demonstration <- asks (FileScheme.demonstration . FileScheme.session . FileScheme.account)
  (registered,)
    <$> ( CommitmentDemonstrated
            <$> ( readFrom (sessionDirectory </> [reldir|demonstrated|] </> FileScheme.preparedMessage demonstration)
                    >>= whenNothingThrow [SessionMessageNotFound (show userWalletPublicKey) (commitmentIdToString commitmentId)]
                )
            <*> ( readFrom (sessionDirectory </> [reldir|demonstrated|] </> FileScheme.scalars demonstration)
                    >>= whenNothingThrow [EncryptionKeysNotFound (show userWalletPublicKey)]
                )
            <*> ( readFrom (sessionDirectory </> [reldir|demonstrated|] </> FileScheme.commitment demonstration)
                    >>= whenNothingThrow [EncryptionKeysNotFound (show userWalletPublicKey)]
                )
        )

commitmentIdToString :: CommitmentId -> String
commitmentIdToString commitmentId =
  let SessionId sessionId = deriveId commitmentId
   in sessionId
