module WBPS.Core.Primitives.SnarkjsOverFileScheme (
  getGenerateBuildCommitmentWitnessProcess,
  toBuildCommitmentWitnessScheme,
) where

import Control.Monad.RWS (MonadReader, ask)
import Path (toFilePath, (</>))
import Shh (Proc)
import WBPS.Core.FileScheme (
  Account (session),
  BuildCommitment (BuildCommitment, input, output),
  BuildCommitmentSetup (BuildCommitmentSetup, wasm),
  FileScheme (account, setup),
 )
import WBPS.Core.FileScheme qualified as Session (Session (commitment))
import WBPS.Core.FileScheme qualified as Setup (Setup (commitment))
import WBPS.Core.Primitives.Snarkjs qualified as Snarkjs
import WBPS.Core.Registration.FileScheme.Directories qualified as Directory

getGenerateBuildCommitmentWitnessProcess :: MonadReader FileScheme m => Directory.Account -> m (Proc ())
getGenerateBuildCommitmentWitnessProcess accountDirectory =
  do
    setup <- Setup.commitment . setup <$> ask
    buildCommitmentSession <- Session.commitment . session . account <$> ask
    return $
      Snarkjs.generateWitness
        (toBuildCommitmentWitnessScheme accountDirectory setup buildCommitmentSession)

toBuildCommitmentWitnessScheme :: Directory.Account -> BuildCommitmentSetup -> BuildCommitment -> Snarkjs.WitnessScheme
toBuildCommitmentWitnessScheme accountDirectory BuildCommitmentSetup {wasm} BuildCommitment {input, output} =
  Snarkjs.WitnessScheme
    { wasm = Path.toFilePath wasm
    , input = Path.toFilePath (accountDirectory </> input)
    , witnessOutput = Path.toFilePath (accountDirectory </> output)
    }
