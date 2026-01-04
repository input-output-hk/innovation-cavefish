{-# LANGUAGE QuasiQuotes #-}

-- | Module for fetching and loading user accounts from the file system.
-- This module provides functions to load existing accounts, load a specific account,
-- and retrieve all recorded user wallet public keys. It handles errors related to
-- missing encryption keys and uses a file scheme for directory structure.
module WBPS.Core.Session.FetchSession (
  loadSession,
  loadExistingSession,
  loadSessions,
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
import WBPS.Core.Cardano.UnsignedTx (toAbstractUnsignedTx)
import WBPS.Core.Failure (RegistrationFailed (AccountNotFound, EncryptionKeysNotFound, SessionMessageNotFound))
import WBPS.Core.FileScheme (FileScheme)
import WBPS.Core.FileScheme qualified as FileScheme
import WBPS.Core.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Registration.Account (AccountCreated (AccountCreated, userWalletPublicKey))
import WBPS.Core.Registration.FetchAccounts (loadAccount, loadAccounts)
import WBPS.Core.Registration.FileScheme (deriveAccountDirectoryFrom)
import WBPS.Core.Session.Commitment (CommitmentId)
import WBPS.Core.Session.FileScheme (deriveExistingSessionDirectoryFrom)
import WBPS.Core.Session.Session (Session (SessionCreated))
import WBPS.Core.ZK.Message (PublicMessage (PublicMessage), unMessage)

getRecordedCommitmentIds :: MonadIO m => Path b Dir -> m [CommitmentId]
getRecordedCommitmentIds p = do
  a <- fst <$> (liftIO . listDirRel $ p)
  return $ fromString . takeWhile (/= '/') . toFilePath <$> a

loadSessions ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) => m [Session]
loadSessions = do
  ( loadAccounts
      >>= mapM
        ( \AccountCreated {userWalletPublicKey} -> do
            accountDir <- deriveAccountDirectoryFrom userWalletPublicKey
            FileScheme.Account {sessions} <- asks FileScheme.account
            recordedIds <- getRecordedCommitmentIds (accountDir </> sessions)
            traverse (loadExistingSession userWalletPublicKey) recordedIds
        )
    )
    <&> join

loadSession ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  UserWalletPublicKey -> CommitmentId -> m (Maybe Session)
loadSession userWalletPublicKey commitmentId = do
  account <- deriveAccountDirectoryFrom userWalletPublicKey
  ifM
    (not <$> doesDirExist account)
    (return Nothing)
    (Just <$> loadExistingSession userWalletPublicKey commitmentId)

loadExistingSession ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  UserWalletPublicKey -> CommitmentId -> m Session
loadExistingSession userWalletPublicKey commitmentId =
  loadAccount userWalletPublicKey
    >>= \case
      Nothing -> throwError [AccountNotFound userWalletPublicKey]
      Just account -> do
        sessionDirectory <- deriveExistingSessionDirectoryFrom userWalletPublicKey commitmentId
        FileScheme.Session
          { message = messageDir
          , rho = rhoDir
          , commitment = FileScheme.BuildCommitment {scalars = scalarsDir, commitment = commitmentDir}
          } <-
          asks (FileScheme.session . FileScheme.account)
        message <- readFrom (sessionDirectory </> messageDir) >>= whenNothingThrow [SessionMessageNotFound userWalletPublicKey commitmentId]
        SessionCreated
          account
          message
          (PublicMessage . toAbstractUnsignedTx . unMessage $ message)
          <$> (readFrom (sessionDirectory </> rhoDir) >>= whenNothingThrow [EncryptionKeysNotFound userWalletPublicKey])
          <*> (readFrom (sessionDirectory </> [reldir|commitment|] </> scalarsDir) >>= whenNothingThrow [EncryptionKeysNotFound userWalletPublicKey])
          <*> (readFrom (sessionDirectory </> [reldir|commitment|] </> commitmentDir) >>= whenNothingThrow [EncryptionKeysNotFound userWalletPublicKey])
