{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

{- HLINT ignore "Functor law" -}

-- | Module for fetching and loading user accounts from the file system.
-- This module provides functions to load existing accounts, load a specific account,
-- and retrieve all recorded user wallet public keys. It handles errors related to
-- missing encryption keys and uses a file scheme for directory structure.
module WBPS.Core.Registration.FetchAccounts (
  loadRegisteredMaybe,
  -- | Load a specific account if it exists
  loadRegistered,
  -- | Load a specific account
  loadExistingRegistered,
  -- | Load an existing account
  loadAllRegistered,
  -- | Load all existing registered accounts
) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask, asks)
import Data.String (IsString (fromString))
import Path (Dir, Path, reldir, toFilePath, (</>))
import Path.IO (doesDirExist, listDirRel)
import WBPS.Adapter.Monad.Control (ifM, whenNothingThrow)
import WBPS.Adapter.Path (readFrom)
import WBPS.Core.Failure (WBPSFailure (AccountNotFound, EncryptionKeysNotFound))
import WBPS.Core.Registration.Artefacts.Groth16.Setup (
  PublicVerificationContext (PublicVerificationContext),
  PublicVerificationContextAsJSON (PublicVerificationContextAsJSON),
  Setup (Setup),
 )
import WBPS.Core.Registration.Persistence.FileScheme (deriveAccountDirectoryFrom)
import WBPS.Core.Registration.Registered (Registered (Registered))
import WBPS.Core.Registration.RegistrationId (RegistrationId)
import WBPS.Core.Setup.Circuit.FileScheme (
  FileScheme (FileScheme, account),
  Registration (
    Registration,
    encryptionKeys,
    provingKey,
    userPublicKey,
    verificationContext
  ),
 )
import WBPS.Core.Setup.Circuit.FileScheme qualified as FileScheme

getRecordedUserWalletPublicKeys :: MonadIO m => Path b Dir -> m [RegistrationId]
getRecordedUserWalletPublicKeys p = do
  a <- fst <$> (liftIO . listDirRel $ p)
  return $ fromString . takeWhile (/= '/') . toFilePath <$> a

loadAllRegistered ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  m [Registered]
loadAllRegistered = do
  FileScheme {..} <- ask
  recordedKeys <- getRecordedUserWalletPublicKeys accounts
  traverse loadExistingRegistered recordedKeys

loadRegisteredMaybe ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  RegistrationId -> m (Maybe Registered)
loadRegisteredMaybe registrationId = do
  account <- deriveAccountDirectoryFrom registrationId
  ifM
    (not <$> doesDirExist account)
    (return Nothing)
    (Just <$> loadExistingRegistered registrationId)

loadRegistered ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  RegistrationId -> m Registered
loadRegistered registrationId =
  loadRegisteredMaybe registrationId >>= whenNothingThrow [AccountNotFound registrationId]

loadExistingRegistered ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  RegistrationId -> m Registered
loadExistingRegistered registrationId = do
  accountDirectory <- deriveAccountDirectoryFrom registrationId
  FileScheme.Account {registration = Registration {..}} <- asks account
  Registered registrationId
    <$> ( Setup (accountDirectory </> [reldir|registered|] </> provingKey)
            <$> (readFrom (accountDirectory </> [reldir|registered|] </> encryptionKeys) >>= whenNothingThrow [EncryptionKeysNotFound registrationId])
            <*> ( PublicVerificationContext (accountDirectory </> [reldir|registered|] </> verificationContext)
                    <$> ( PublicVerificationContextAsJSON
                            <$> ( readFrom (accountDirectory </> [reldir|registered|] </> verificationContext)
                                    >>= whenNothingThrow [EncryptionKeysNotFound registrationId]
                                )
                        )
                )
        )
