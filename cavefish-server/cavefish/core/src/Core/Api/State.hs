module Core.Api.State where

import Cardano.Api (ConwayEra, Tx, TxId)
import Cooked.MockChain.MockChainState (MockChainState)
import Core.Pke (PkeCiphertext)
import Core.Proof (parseHex, renderHex)
import Crypto.Error (CryptoFailable (..))
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
  { signerPublicKey :: PublicKey
  }
  deriving (Eq, Show, Generic)

instance FromJSON ClientRegistration where
  parseJSON = withObject "ClientRegistration" $ \o -> do
    signerPublicKey <- parsePublicKey =<< o .: "signerPublicKey"
    pure ClientRegistration {signerPublicKey}

instance ToJSON ClientRegistration where
  toJSON ClientRegistration {signerPublicKey} =
    object
      [ "signerPublicKey" .= renderHex (renderPublicKey signerPublicKey)
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
