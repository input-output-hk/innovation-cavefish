module WBPS.Core.Registration.SnarkJs.OverFileSchemeAndShh (
  getGenerateProvingKeyProcess,
  getGenerateVerificationKeyProcess,
) where

import Control.Monad.RWS (MonadReader, asks)
import Path (toFilePath, (</>))
import Shh (Proc)
import WBPS.Core.FileScheme (
  FileScheme (
    FileScheme,
    powerOfTauPrepared,
    provingKey,
    relationR1CS,
    verificationContext,
    witnessInput,
    witnessOutput
  ),
 )
import WBPS.Core.Registration.FileScheme.Directories qualified as Directory
import WBPS.Core.Registration.SnarkJs.OverShh qualified as Snarkjs

getGenerateProvingKeyProcess :: MonadReader FileScheme m => Directory.Account -> m (Proc ())
getGenerateProvingKeyProcess account = asks (Snarkjs.generateProvingKey . toProvingKeyScheme account)

getGenerateVerificationKeyProcess :: MonadReader FileScheme m => Directory.Account -> m (Proc ())
getGenerateVerificationKeyProcess account = asks (Snarkjs.generateVerificationKey . toVerificationKeyScheme account)

toProvingKeyScheme :: Directory.Account -> FileScheme -> Snarkjs.ProvingKeyScheme
toProvingKeyScheme account FileScheme {..} =
  Snarkjs.ProvingKeyScheme
    { powerOfTauPrepared = Path.toFilePath powerOfTauPrepared
    , relationR1CS = Path.toFilePath relationR1CS
    , provingKeyOutput = Path.toFilePath (account </> provingKey)
    }

toVerificationKeyScheme :: Directory.Account -> FileScheme -> Snarkjs.VerificationKeyScheme
toVerificationKeyScheme account FileScheme {..} =
  Snarkjs.VerificationKeyScheme
    { provingKey = Path.toFilePath (account </> provingKey)
    , verificationKeyOutput = Path.toFilePath (account </> verificationContext)
    }
