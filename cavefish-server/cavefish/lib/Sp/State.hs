module Sp.State where

import Cardano.Api
import Cooked.MockChain.MockChainState (MockChainState)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Time (UTCTime)
import GHC.Conc (TVar)

data Pending = Pending
  { tx :: Tx ConwayEra
  , txAbsHash :: ByteString
  , expiry :: UTCTime
  , mockState :: MockChainState
  }

type PendingStore = TVar (Map TxId Pending)
