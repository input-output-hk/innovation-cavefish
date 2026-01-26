module WBPS.Core.Session.Steps.Proving.Proved (
  CommitmentProved (..),
) where

import GHC.Generics (Generic)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.R (R)
import WBPS.Core.Session.Steps.Proving.Artefacts.Challenge (Challenge)
import WBPS.Core.Session.Steps.Proving.Artefacts.Proof (Proof)

data CommitmentProved
  = CommitmentProved
  { bigR :: R
  , challenge :: Challenge
  , proof :: Proof
  }
  deriving (Eq, Show, Generic)
