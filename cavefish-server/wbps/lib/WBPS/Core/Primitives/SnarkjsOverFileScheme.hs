module WBPS.Core.Primitives.SnarkjsOverFileScheme (
  getGenerateProvingKeyProcess,
  getGenerateVerificationKeyProcess,
) where

import Control.Monad.RWS (MonadReader, asks)
import Path
import Path.IO
import Shh (Proc)
import WBPS.Core.FileScheme
import WBPS.Core.Primitives.Snarkjs qualified as Snarkjs

getGenerateProvingKeyProcess :: MonadReader FileScheme m => Account -> m (Proc ())
getGenerateProvingKeyProcess account = asks (Snarkjs.generateProvingKey . toProvingKeyScheme account)

getGenerateVerificationKeyProcess :: MonadReader FileScheme m => Account -> m (Proc ())
getGenerateVerificationKeyProcess account = asks (Snarkjs.generateVerificationKey . toVerificationKeyScheme account)

toProvingKeyScheme :: Account -> FileScheme -> Snarkjs.ProvingKeyScheme
toProvingKeyScheme account FileScheme {..} =
  Snarkjs.ProvingKeyScheme
    { powerOfTauPrepared = Path.toFilePath powerOfTauPrepared
    , relationR1CS = Path.toFilePath relationR1CS
    , provingKeyOutput = Path.toFilePath (account </> provingKey)
    }

toVerificationKeyScheme :: Account -> FileScheme -> Snarkjs.VerificationKeyScheme
toVerificationKeyScheme account FileScheme {..} =
  Snarkjs.VerificationKeyScheme
    { provingKey = Path.toFilePath (account </> provingKey)
    , verificationKeyOutput = Path.toFilePath (account </> verificationKey)
    }
