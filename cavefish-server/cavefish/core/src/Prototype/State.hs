module Prototype.State (Pending (..), PendingStore, Completed (..), CompleteStore) where

import Cardano.Api (ConwayEra, Tx, TxId)
import Cooked.MockChain.MockChainState (MockChainState)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Time (UTCTime)
import GHC.Conc (TVar)
import GHC.Generics (Generic)
import Prototype.Pke (PkeCiphertext)
import WBPS.Core.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Keys.Ed25519 qualified as Ed25519
import WBPS.Core.Session.Commitment.Build (ComId)

data Pending = Pending
  { tx :: Tx ConwayEra
  , txAbsHash :: ByteString
  , expiry :: UTCTime
  , mockState :: MockChainState
  , creator :: UserWalletPublicKey
  , ciphertext :: PkeCiphertext
  , auxNonce :: ByteString
  , rho :: ByteString
  , message :: ByteString
  , comId :: ComId
  , comTx :: [Integer]
  , commitment :: Maybe Ed25519.PublicKey
  , challenge :: Maybe ByteString
  }

type PendingStore = TVar (Map TxId Pending)

data Completed = Completed
  { tx :: Tx ConwayEra
  , submittedAt :: UTCTime
  , creator :: UserWalletPublicKey
  }
  deriving (Eq, Show, Generic)

type CompleteStore = TVar (Map TxId Completed)
