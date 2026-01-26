module WBPS.Core.Session.Steps.Demonstration.Demonstrated (
  CommitmentDemonstrated (..),
) where

import GHC.Generics (Generic)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Commitment (Commitment)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.PreparedMessage (PreparedMessage)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Scalars (Scalars)

data CommitmentDemonstrated
  = CommitmentDemonstrated
  { preparedMessage :: PreparedMessage
  , scalars :: Scalars
  , commitment :: Commitment
  }
  deriving (Eq, Show, Generic)
