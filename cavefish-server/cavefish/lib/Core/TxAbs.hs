module Core.TxAbs where

import Cardano.Api
import Cooked (TxSkel)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Ledger (Interval, PubKey, Slot)

data TxAbs era = TxAbs
  { txAbsOutputs :: [TxOut CtxTx era]
  , txAbsValidityInterval :: Interval Slot
  , txAbsMint :: Value
  , txAbsFee :: Integer
  , txAbsSigKeys :: Set.Set PubKey
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

fromTxSkel :: TxSkel -> TxAbs era
fromTxSkel = undefined

mkTxAbs :: Tx era -> TxAbs era
mkTxAbs = undefined