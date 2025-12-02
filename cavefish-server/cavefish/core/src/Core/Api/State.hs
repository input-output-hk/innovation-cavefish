module Core.Api.State where

import Cardano.Api (ConwayEra, Tx, TxId)
import Cooked.MockChain.MockChainState (MockChainState)
import Core.Pke (PkeCiphertext)
import Core.Proof (parseHex, renderHex)
import Crypto.Error (CryptoFailable (CryptoFailed, CryptoPassed))
import Crypto.PubKey.Ed25519 (PublicKey)
import Crypto.PubKey.Ed25519 qualified as Ed
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, withObject, (.:), (.=))
import Data.Aeson.Types (Parser)
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Text (Text)
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

data ClientRegistration = ClientRegistration
  { userPublicKey :: PublicKey
  , xPublicKey :: WbpsPublicKey
  }
  deriving (Eq, Show, Generic)

instance FromJSON ClientRegistration where
  parseJSON = withObject "ClientRegistration" $ \o -> do
    userPublicKey <- parsePublicKey =<< o .: "userPublicKey"
    xPublicKey <- o .: "X"
    pure ClientRegistration {userPublicKey, xPublicKey}

instance ToJSON ClientRegistration where
  toJSON ClientRegistration {userPublicKey, xPublicKey} =
    object
      [ "userPublicKey" .= renderHex (renderPublicKey userPublicKey)
      , "X" .= xPublicKey
      ]

type ClientRegistrationStore = TVar (Map ClientId ClientRegistration)

type CompleteStore = TVar (Map TxId Completed)

parsePublicKey :: Text -> Parser PublicKey
parsePublicKey vkHex = do
  bytes <- parseHex vkHex
  case Ed.publicKey bytes of
    CryptoFailed _ -> fail "invalid public key"
    CryptoPassed vk -> pure vk

renderPublicKey :: PublicKey -> ByteString
renderPublicKey = BA.convert
