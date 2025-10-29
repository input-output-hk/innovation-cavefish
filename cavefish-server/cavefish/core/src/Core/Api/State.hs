module Core.Api.State where

import Cardano.Api
import Cooked.MockChain.MockChainState (MockChainState)
import Core.Pke (PkeCiphertext)
import Crypto.PubKey.Ed25519 (PublicKey)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Conc (TVar)
import GHC.Generics (Generic)

data Pending = Pending
  { tx :: Tx ConwayEra
  , txAbsHash :: ByteString
  , expiry :: UTCTime
  , mockState :: MockChainState
  , creator :: ClientId
  , ciphertext :: PkeCiphertext
  , auxNonce :: ByteString
  , rho :: ByteString
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

newtype ClientRegistration = ClientRegistration
  { publicKey :: PublicKey
  }
  deriving (Eq, Show, Generic)

instance FromJSON ClientRegistration

instance ToJSON ClientRegistration

type ClientRegistrationStore = TVar (Map ClientId ClientRegistration)

type CompleteStore = TVar (Map TxId Completed)
