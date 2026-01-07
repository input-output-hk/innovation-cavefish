module WBPS.Core.Session.Commitment.Scalars.Compute (
  compute,
) where

import Control.Monad.Except (MonadError)
import WBPS.Core.Failure (RegistrationFailed, toWBPSFailure)
import WBPS.Core.Keys.ElGamal (
  EncryptionKey,
  Rho,
  encryptionKeyPowRho,
  generatorPowRho,
 )
import WBPS.Core.Session.Commitment.Scalars (CommitmentScalars (CommitmentScalars))

compute ::
  MonadError [RegistrationFailed] m => EncryptionKey -> Rho -> m CommitmentScalars
compute ek rho =
  CommitmentScalars
    <$> toWBPSFailure (encryptionKeyPowRho ek rho)
    <*> toWBPSFailure (generatorPowRho rho)
