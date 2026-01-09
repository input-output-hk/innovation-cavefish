-- | Commitment scalar utilities will live here.
module WBPS.Core.Session.Scalars (
  Scalars (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import WBPS.Core.Keys.ElGamal (
  AffinePoint,
  Rho,
 )

data Scalars
  = Scalars
  { rho :: Rho
  , ekPowRho :: AffinePoint
  , gPowRho :: AffinePoint
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)
