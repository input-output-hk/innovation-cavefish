module WBPS.Core.Session.Demonstration.Scalars.Compute (
  compute,
) where

import Control.Monad.Except (MonadError)
import WBPS.Core.Failure (WBPSFailure, toWBPSFailure)
import WBPS.Core.Keys.ElGamal (
  EncryptionKey,
  Rho,
  encryptionKeyPowRho,
  generatorPowRho,
 )
import WBPS.Core.Session.Demonstration.Scalars (Scalars (Scalars))

compute ::
  MonadError [WBPSFailure] m => EncryptionKey -> Rho -> m Scalars
compute ek rho =
  Scalars rho
    <$> toWBPSFailure (encryptionKeyPowRho ek rho)
    <*> toWBPSFailure (generatorPowRho rho)
