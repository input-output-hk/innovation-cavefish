module Sp.State where

import Cardano.Api
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Time (UTCTime)
import GHC.Conc (TVar)

data Pending = Pending
  { pTx :: Tx ConwayEra
  , pTxAbsHash :: ByteString
  , pExpiry :: UTCTime
  }

type PendingStore = TVar (Map TxId Pending)