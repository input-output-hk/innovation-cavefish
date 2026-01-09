module WBPS.Core.Session.Proving.Proved (
  CommitmentProved (..),
) where

import GHC.Generics (Generic)
import WBPS.Core.Session.Demonstration.R (R)
import WBPS.Core.Session.Proving.Challenge (Challenge)
import WBPS.Core.Session.Proving.Proof (Proof)

data CommitmentProved
  = CommitmentProved
  { bigR :: R
  , challenge :: Challenge
  , proof :: Proof
  }
  deriving (Eq, Show, Generic)
