module Core.Api.State (Pending (..), PendingStore, Completed (..), ClientId (..), CompleteStore) where

import Cardano.Api (ConwayEra, Tx, TxId)
import Cooked.MockChain.MockChainState (MockChainState)
import Core.Pke (PkeCiphertext)
import Crypto.PubKey.Ed25519 (PublicKey)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Conc (TVar)
import GHC.Generics (Generic)
import WBPS.Core (WbpsPublicKey)
import WBPS.Core.BuildCommitment (ComId)

data Pending = Pending
  { tx :: Tx ConwayEra
  , txAbsHash :: ByteString
  , expiry :: UTCTime
  , mockState :: MockChainState
  , creator :: ClientId
  , ciphertext :: PkeCiphertext
  , auxNonce :: ByteString
  , rho :: ByteString
  , message :: ByteString
  , comId :: ComId
  , comTx :: [Integer]
  , commitment :: Maybe PublicKey
  , challenge :: Maybe ByteString
  }

type PendingStore = TVar (Map TxId Pending)

data Completed = Completed
  { tx :: Tx ConwayEra
  , submittedAt :: UTCTime
  , creator :: ClientId
  }
  deriving (Eq, Show, Generic)

newtype ClientId = ClientId {unClientId :: UUID}
  deriving (Eq, Show, Ord, Generic)
  deriving newtype (FromJSON, ToJSON)

type CompleteStore = TVar (Map TxId Completed)
