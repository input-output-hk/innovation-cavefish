-- | Commitment scalar utilities will live here.
module WBPS.Core.Session.Steps.Demonstration.Artefacts.Scalars (
  Scalars (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import WBPS.Adapter.Math.AffinePoint (AffinePoint)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Rho (Rho)

data Scalars
  = Scalars
  { rho :: Rho
  , ekPowRho :: AffinePoint
  , gPowRho :: AffinePoint
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)
