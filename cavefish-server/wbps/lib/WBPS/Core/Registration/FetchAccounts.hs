{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

-- | Module for fetching and loading user accounts from the file system.
-- This module provides functions to load existing accounts, load a specific account,
-- and retrieve all recorded user wallet public keys. It handles errors related to
-- missing encryption keys and uses a file scheme for directory structure.
module WBPS.Core.Registration.FetchAccounts (
  loadAccount,
  -- | Load a specific account
  loadExistingAccount,
  -- | Load an existing accountj
  loadAccounts,
  -- | Load all existing accounts
) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask, asks)
import Data.String (IsString (fromString))
import Path (Dir, Path, reldir, toFilePath, (</>))
import Path.IO (doesDirExist, listDirRel)
import WBPS.Adapter.Monad.Control (ifM, whenNothingThrow)
import WBPS.Adapter.Path (readFrom)
import WBPS.Core.Failure (WBPSFailure (EncryptionKeysNotFound))
import WBPS.Core.Registration.Artefacts.Groth16.Setup (PublicVerificationContext (PublicVerificationContext), Setup (Setup))
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Registration.FileScheme (deriveAccountDirectoryFrom)
import WBPS.Core.Registration.Registered (Registered (Registered))
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

getRecordedUserWalletPublicKeys :: MonadIO m => Path b Dir -> m [UserWalletPublicKey]
getRecordedUserWalletPublicKeys p = do
  a <- fst <$> (liftIO . listDirRel $ p)
  return $ fromString . takeWhile (/= '/') . toFilePath <$> a

loadAccounts ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  m [Registered]
loadAccounts = do
  FileScheme {..} <- ask
  recordedKeys <- getRecordedUserWalletPublicKeys accounts
  traverse loadExistingAccount recordedKeys

loadAccount ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  UserWalletPublicKey -> m (Maybe Registered)
loadAccount userWalletPublicKey = do
  account <- deriveAccountDirectoryFrom userWalletPublicKey
  ifM
    (not <$> doesDirExist account)
    (return Nothing)
    (Just <$> loadExistingAccount userWalletPublicKey)

loadExistingAccount ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  UserWalletPublicKey -> m Registered
loadExistingAccount userWalletPublicKey = do
  accountDirectory <- deriveAccountDirectoryFrom userWalletPublicKey
  FileScheme.Account {registration = Registration {..}} <- asks account
  Registered userWalletPublicKey
    <$> ( Setup (accountDirectory </> [reldir|registered|] </> provingKey)
            <$> (readFrom (accountDirectory </> [reldir|registered|] </> encryptionKeys) >>= whenNothingThrow [EncryptionKeysNotFound . show $ userWalletPublicKey])
            <*> ( PublicVerificationContext (accountDirectory </> [reldir|registered|] </> verificationContext)
                    <$> ( readFrom (accountDirectory </> [reldir|registered|] </> verificationContext)
                            >>= whenNothingThrow [EncryptionKeysNotFound . show $ userWalletPublicKey]
                        )
                )
        )
