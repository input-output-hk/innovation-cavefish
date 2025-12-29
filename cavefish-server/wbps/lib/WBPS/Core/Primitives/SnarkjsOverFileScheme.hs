module WBPS.Core.Primitives.SnarkjsOverFileScheme (
  getGenerateBuildCommitmentWitnessProcess,
  toBuildCommitmentWitnessScheme,
) where

import Control.Monad.RWS (MonadReader, asks)
import Path (toFilePath, (</>))
import Shh (Proc)
import WBPS.Core.FileScheme (
  FileScheme (
    FileScheme,
    buildCommitmentWASM,
    powerOfTauPrepared,
    provingKey,
    relationR1CS,
    verificationContext,
    witnessInput,
    witnessOutput
  ),
 )
import WBPS.Core.Primitives.Snarkjs qualified as Snarkjs
import WBPS.Core.Registration.FileScheme.Directories qualified as Directory

getGenerateBuildCommitmentWitnessProcess :: MonadReader FileScheme m => Directory.Account -> m (Proc ())
getGenerateBuildCommitmentWitnessProcess account = asks (Snarkjs.generateWitness . toBuildCommitmentWitnessScheme account)

toBuildCommitmentWitnessScheme :: Directory.Account -> FileScheme -> Snarkjs.WitnessScheme
toBuildCommitmentWitnessScheme account FileScheme {..} =
  Snarkjs.WitnessScheme
    { wasm = Path.toFilePath buildCommitmentWASM
    , input = Path.toFilePath (account </> witnessInput)
    , witnessOutput = Path.toFilePath (account </> witnessOutput)
    }
