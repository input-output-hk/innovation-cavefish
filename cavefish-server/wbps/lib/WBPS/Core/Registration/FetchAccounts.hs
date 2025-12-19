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
import Control.Monad.Reader (MonadReader, ask)
import Data.String (IsString (fromString))
import Path (Dir, Path, toFilePath, (</>))
import Path.IO (doesDirExist, listDirRel)
import WBPS.Adapter.Monad.Control (ifM, whenNothingThrow)
import WBPS.Adapter.Path (readFrom)
import WBPS.Core.Failure (RegistrationFailed (EncryptionKeysNotFound))
import WBPS.Core.FileScheme (FileScheme (FileScheme, accounts, encryptionKeys, provingKey, verificationContext))
import WBPS.Core.Groth16.Setup (PublicVerificationContext (PublicVerificationContext), Setup (Setup))
import WBPS.Core.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Registration.Account (AccountCreated (AccountCreated))
import WBPS.Core.Registration.FileScheme (deriveDirectoryAccountFrom)

getRecordedUserWalletPublicKeys :: MonadIO m => Path b Dir -> m [UserWalletPublicKey]
getRecordedUserWalletPublicKeys p = do
  a <- fst <$> (liftIO . listDirRel $ p)
  return $ fromString . takeWhile (/= '/') . toFilePath <$> a

loadAccounts ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  m [AccountCreated]
loadAccounts = do
  FileScheme {..} <- ask
  recordedKeys <- getRecordedUserWalletPublicKeys accounts
  traverse loadExistingAccount recordedKeys

loadAccount ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  UserWalletPublicKey -> m (Maybe AccountCreated)
loadAccount userWalletPublicKey = do
  account <- deriveDirectoryAccountFrom userWalletPublicKey
  ifM
    (not <$> doesDirExist account)
    (return Nothing)
    (Just <$> loadExistingAccount userWalletPublicKey)

loadExistingAccount ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  UserWalletPublicKey -> m AccountCreated
loadExistingAccount userWalletPublicKey = do
  account <- deriveDirectoryAccountFrom userWalletPublicKey
  FileScheme {..} <- ask
  AccountCreated userWalletPublicKey
    <$> ( Setup (account </> provingKey)
            <$> (readFrom (account </> encryptionKeys) >>= whenNothingThrow [EncryptionKeysNotFound userWalletPublicKey])
            <*> ( PublicVerificationContext (account </> verificationContext)
                    <$> (readFrom (account </> verificationContext) >>= whenNothingThrow [EncryptionKeysNotFound userWalletPublicKey])
                )
        )
