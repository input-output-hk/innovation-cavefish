-- | Commitment scalar utilities will live here.
module WBPS.Core.Session.Commitment.Scalars (
  CommitmentScalars (..),
) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import WBPS.Core.Keys.ElGamal (
  AffinePoint,
 )

data CommitmentScalars
  = CommitmentScalars
  { ekPowRho :: AffinePoint
  , gPowRho :: AffinePoint
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)
