module WBPS.Core.Session.Demonstration.Artefacts.Scalars.Compute (
  compute,
) where

import Control.Monad.Except (MonadError)
import WBPS.Core.Failure (WBPSFailure, toWBPSFailure)
import WBPS.Core.Registration.Artefacts.Keys.ElGamal (EncryptionKey)
import WBPS.Core.Session.Demonstration.Artefacts.Rho (
  Rho,
  encryptionKeyPowRho,
  generatorPowRho,
 )
import WBPS.Core.Session.Demonstration.Artefacts.Scalars (Scalars (Scalars))

compute ::
  MonadError [WBPSFailure] m => EncryptionKey -> Rho -> m Scalars
compute ek rho =
  Scalars rho
    <$> toWBPSFailure (encryptionKeyPowRho ek rho)
    <*> toWBPSFailure (generatorPowRho rho)
