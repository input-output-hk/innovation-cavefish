module WBPS.Core.Registration.SnarkJs.OverFileSchemeAndShh (
  getGenerateProvingKeyProcess,
  getGenerateVerificationKeyProcess,
) where

import Control.Monad.RWS (MonadReader, asks)
import Path (toFilePath, (</>))
import Shh (Proc)
import WBPS.Core.FileScheme (
  Account (
    Account,
    encryptionKeys,
    provingKey,
    userPublicKey,
    verificationContext
  ),
  FileScheme (FileScheme, account),
  Setup (Setup, powerOfTauPrepared, relationR1CS),
 )
import WBPS.Core.FileScheme qualified as FileScheme
import WBPS.Core.Registration.FileScheme.Directories qualified as Directory
import WBPS.Core.Registration.SnarkJs.OverShh qualified as Snarkjs

getGenerateProvingKeyProcess :: MonadReader FileScheme m => Directory.Account -> m (Proc ())
getGenerateProvingKeyProcess accountDirectory = asks (Snarkjs.generateProvingKey . toProvingKeyScheme accountDirectory)

getGenerateVerificationKeyProcess :: MonadReader FileScheme m => Directory.Account -> m (Proc ())
getGenerateVerificationKeyProcess accountDirectory = asks (Snarkjs.generateVerificationKey . toVerificationKeyScheme accountDirectory . account)

toProvingKeyScheme :: Directory.Account -> FileScheme -> Snarkjs.ProvingKeyScheme
toProvingKeyScheme
  accountDirectory
  FileScheme
    { account = Account {..}
    , setup = Setup {powerOfTauPrepared, relationR1CS}
    } =
    Snarkjs.ProvingKeyScheme
      { powerOfTauPrepared = Path.toFilePath powerOfTauPrepared
      , relationR1CS = Path.toFilePath relationR1CS
      , provingKeyOutput = Path.toFilePath (accountDirectory </> provingKey)
      }

toVerificationKeyScheme :: Directory.Account -> FileScheme.Account -> Snarkjs.VerificationKeyScheme
toVerificationKeyScheme accountDirectory FileScheme.Account {..} =
  Snarkjs.VerificationKeyScheme
    { provingKey = Path.toFilePath (accountDirectory </> provingKey)
    , verificationKeyOutput = Path.toFilePath (accountDirectory </> verificationContext)
    }
