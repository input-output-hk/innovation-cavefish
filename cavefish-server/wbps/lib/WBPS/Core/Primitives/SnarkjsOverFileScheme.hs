module WBPS.Core.Primitives.SnarkjsOverFileScheme (
  getGenerateBuildCommitmentWitnessProcess,
  toBuildCommitmentWitnessScheme,
) where

import Control.Monad.RWS (MonadReader, ask)
import Path (toFilePath, (</>))
import Shh (Proc)
import WBPS.Core.Primitives.Snarkjs qualified as Snarkjs
import WBPS.Core.Registration.FileScheme.Directories qualified as Directory
import WBPS.Core.Setup.Circuit.FileScheme (
  Account (session),
  BuildCommitmentInternals (BuildCommitmentInternals, input, output),
  BuildCommitmentSetup (BuildCommitmentSetup, wasm),
  Demonstration (buildCommitmentInternals),
  FileScheme (account, setup),
  Session (demonstration),
  Setup (buildCommitment),
 )

getGenerateBuildCommitmentWitnessProcess :: MonadReader FileScheme m => Directory.Account -> m (Proc ())
getGenerateBuildCommitmentWitnessProcess accountDirectory =
  do
    setup <- buildCommitment . setup <$> ask
    internals <- buildCommitmentInternals . demonstration . session . account <$> ask
    return $
      Snarkjs.generateWitness
        (toBuildCommitmentWitnessScheme accountDirectory setup internals)

toBuildCommitmentWitnessScheme :: Directory.Account -> BuildCommitmentSetup -> BuildCommitmentInternals -> Snarkjs.WitnessScheme
toBuildCommitmentWitnessScheme accountDirectory BuildCommitmentSetup {wasm} BuildCommitmentInternals {input, output} =
  Snarkjs.WitnessScheme
    { wasm = Path.toFilePath wasm
    , input = Path.toFilePath (accountDirectory </> input)
    , witnessOutput = Path.toFilePath (accountDirectory </> output)
    }
