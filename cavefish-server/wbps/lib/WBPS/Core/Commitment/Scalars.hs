-- | Commitment scalar utilities will live here.
module WBPS.Core.Commitment.Scalars (
  compute,
  CommitmentScalars (..),
) where

import Control.Monad.Except (MonadError)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import WBPS.Core.Failure (RegistrationFailed, toWBPSFailure)
import WBPS.Core.Keys.ElGamal (
  AffinePoint,
  EncryptionKey,
  Rho,
  encryptionKeyPowRho,
  generatorPowRho,
 )

data CommitmentScalars
  = CommitmentScalars
  { ekPowRho :: AffinePoint
  , gPowRho :: AffinePoint
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

compute ::
  MonadError [RegistrationFailed] m => EncryptionKey -> Rho -> m CommitmentScalars
compute ek rho =
  CommitmentScalars
    <$> toWBPSFailure (encryptionKeyPowRho ek rho)
    <*> toWBPSFailure (generatorPowRho rho)
