module WBPS.Core.Session.Scalars.Compute (
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
import WBPS.Core.Session.Scalars (Scalars (Scalars))

compute ::
  MonadError [RegistrationFailed] m => EncryptionKey -> Rho -> m Scalars
compute ek rho =
  Scalars
    <$> pure rho
    <*> toWBPSFailure (encryptionKeyPowRho ek rho)
    <*> toWBPSFailure (generatorPowRho rho)
