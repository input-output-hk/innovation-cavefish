{-# LANGUAGE QuasiQuotes #-}

module WBPS.Core.Registration.Persistence.SnarkJs.OverFileSchemeAndShh (
  getGenerateProvingKeyProcess,
  getGenerateVerificationKeyProcess,
) where

import Control.Monad.RWS (MonadReader, asks)
import Path (reldir, toFilePath, (</>))
import Shh (Proc)
import WBPS.Core.Registration.Persistence.FileScheme.Directories qualified as Directory
import WBPS.Core.Registration.Persistence.SnarkJs.OverShh qualified as Snarkjs
import WBPS.Core.Setup.Circuit.FileScheme (
  Account (
    Account,
    registration
  ),
  FileScheme (FileScheme, account),
  Registration (
    Registration,
    encryptionKeys,
    provingKey,
    userPublicKey,
    verificationContext
  ),
  Setup (Setup, powerOfTauPrepared, relationR1CS),
 )
import WBPS.Core.Setup.Circuit.FileScheme qualified as FileScheme

getGenerateProvingKeyProcess :: MonadReader FileScheme m => Directory.Account -> m (Proc ())
getGenerateProvingKeyProcess accountDirectory = asks (Snarkjs.generateProvingKey . toProvingKeyScheme accountDirectory)

getGenerateVerificationKeyProcess :: MonadReader FileScheme m => Directory.Account -> m (Proc ())
getGenerateVerificationKeyProcess accountDirectory = asks (Snarkjs.generateVerificationKey . toVerificationKeyScheme accountDirectory . account)

toProvingKeyScheme :: Directory.Account -> FileScheme -> Snarkjs.ProvingKeyScheme
toProvingKeyScheme
  accountDirectory
  FileScheme
    { account = Account {registration = Registration {..}}
    , setup = Setup {powerOfTauPrepared, relationR1CS}
    } =
    Snarkjs.ProvingKeyScheme
      { powerOfTauPrepared = Path.toFilePath powerOfTauPrepared
      , relationR1CS = Path.toFilePath relationR1CS
      , provingKeyOutput = Path.toFilePath (accountDirectory </> [reldir|registered|] </> provingKey)
      }

toVerificationKeyScheme :: Directory.Account -> FileScheme.Account -> Snarkjs.VerificationKeyScheme
toVerificationKeyScheme accountDirectory FileScheme.Account {registration = Registration {..}} =
  Snarkjs.VerificationKeyScheme
    { provingKey = Path.toFilePath (accountDirectory </> [reldir|registered|] </> provingKey)
    , verificationKeyOutput = Path.toFilePath (accountDirectory </> [reldir|registered|] </> verificationContext)
    }
