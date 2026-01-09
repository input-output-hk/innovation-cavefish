module WBPS.Core.Session.Demonstration.Demonstrated (
  CommitmentDemonstrated (..),
) where

import GHC.Generics (Generic)
import WBPS.Core.Session.Demonstration.Commitment (Commitment)
import WBPS.Core.Session.Demonstration.Message (PreparedMessage)
import WBPS.Core.Session.Demonstration.Scalars (Scalars)

data CommitmentDemonstrated
  = CommitmentDemonstrated
  { preparedMessage :: PreparedMessage
  , scalars :: Scalars
  , commitment :: Commitment
  }
  deriving (Eq, Show, Generic)
