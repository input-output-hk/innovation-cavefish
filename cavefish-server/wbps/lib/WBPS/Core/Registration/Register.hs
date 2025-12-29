{-# LANGUAGE RecordWildCards #-}

-- | Module      : WBPS.Core.Registration.Register
--     Description : Functionality to register a new user account within the WBPS system.
module WBPS.Core.Registration.Register (
  register,
  -- | Register a new user account
) where

import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask)
import Path ((</>))
import Path.IO (ensureDir)
import Shh (Stream (Append, StdOut), (&!>), (&>))
import WBPS.Adapter.Path (writeTo)
import WBPS.Core.Failure (RegistrationFailed (AccountAlreadyRegistered))
import WBPS.Core.FileScheme (FileScheme (FileScheme, encryptionKeys), getShellLogsFilepath)
import WBPS.Core.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Keys.ElGamal qualified as ElGamal
import WBPS.Core.Registration.Account (
  AccountCreated (AccountCreated, userWalletPublicKey),
 )
import WBPS.Core.Registration.FetchAccounts (loadAccount, loadExistingAccount)
import WBPS.Core.Registration.FileScheme (deriveDirectoryAccountFrom)
import WBPS.Core.Registration.FileScheme.Directories qualified as Directory
import WBPS.Core.Registration.SnarkJs.OverFileSchemeAndShh (getGenerateProvingKeyProcess, getGenerateVerificationKeyProcess)

register ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  UserWalletPublicKey ->
  m AccountCreated
register userWalletPublicKey =
  loadAccount userWalletPublicKey
    >>= \case
      Just AccountCreated {userWalletPublicKey = existingUserWalletKey} -> throwError [AccountAlreadyRegistered existingUserWalletKey]
      Nothing -> do
        register' =<< deriveDirectoryAccountFrom userWalletPublicKey
        loadExistingAccount userWalletPublicKey

register' ::
  (MonadIO m, MonadReader FileScheme m) =>
  Directory.Account ->
  m ()
register' account = do
  FileScheme {..} <- ask
  ensureDir account
  ElGamal.generateKeyPair >>= writeTo (account </> encryptionKeys)
  generateProvingKeyProcess <- getGenerateProvingKeyProcess account
  generateVerificationKeyProcess <- getGenerateVerificationKeyProcess account
  shellLogsFilepath <- getShellLogsFilepath account
  liftIO $
    (generateProvingKeyProcess >> generateVerificationKeyProcess)
      &!> StdOut
      &> Append shellLogsFilepath
