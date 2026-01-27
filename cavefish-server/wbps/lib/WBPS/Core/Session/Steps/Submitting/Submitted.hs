module WBPS.Core.Session.Steps.Submitting.Submitted (
  CommitmentSubmitted (..),
) where

import Cardano.Api (TxId)
import GHC.Generics (Generic)
import WBPS.Core.Session.Steps.BlindSigning.BlindSignature (BlindSignature)
import WBPS.Core.Session.Steps.Submitting.Artefacts.SubmittedTx (SubmittedTx)
import WBPS.Core.Session.Steps.Submitting.Artefacts.TxSignature (TxSignature)

data CommitmentSubmitted
  = CommitmentSubmitted
  { blindSignature :: BlindSignature
  , txSignature :: TxSignature
  , submittedTx :: SubmittedTx
  , txId :: TxId
  }
  deriving (Eq, Show, Generic)
