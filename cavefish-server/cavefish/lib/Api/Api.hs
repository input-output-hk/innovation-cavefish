{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Api where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Data.Aeson
import Data.Monoid
import Data.Proxy
import Data.Text (Text)
import GHC.Generics
import Servant.API

import qualified Data.Text    as T
import qualified Data.Text.IO as T
import Control.Monad.Trans.Either
import Servant
import Data.Time
import qualified Cardano.Api as Api
import NodeClient.NodeClient
import Control.Monad.Trans.State
import GHC.Clock (getMonotonicTimeNSec)
import qualified Data.Map as Map

type CavefishApi =
       "v1" :> "health" :> Get '[JSON] Health
  :<|> "v1" :> "info" :> Get '[JSON] Info
  :<|> "v1" :> "tip" :> Get '[JSON] Tip
  :<|> "v1" :> "utxos-for-addr"
        :> Capture "addr" Text
          :> Get '[JSON] [(Api.TxIn, Api.TxOut Api.CtxUTxO Api.ConwayEra)]

data Health = Health { status :: Integer, uptimeS :: Integer } deriving (Show, Generic, ToJSON, FromJSON)

instance Semigroup Health where
  Health s1 t1 <> Health s2 t2 =
    Health { status = s1 + s2, uptimeS = t1 + t2 }

instance Monoid Health where
  mempty = Health { status = 0, uptimeS = 0 }

data Network = Preprod
  deriving stock (Show, Generic)

instance ToJSON Network where
  toJSON = genericToJSON defaultOptions
    { tagSingleConstructors = True
    , sumEncoding = UntaggedValue
    }

instance FromJSON Network where
  parseJSON = genericParseJSON defaultOptions
    { tagSingleConstructors = True
    , sumEncoding = UntaggedValue
    }
    
data Info = Info { network :: Network } deriving (Show, Generic, ToJSON, FromJSON)

data Tip = Tip { slot :: Api.SlotNo, height :: Integer, blockHash :: Maybe Hex } deriving (Show, Eq, Generic, ToJSON, FromJSON)

newtype Env = Env { uptimeS :: Integer }

instance Semigroup Tip where
  Tip s1 h1 b1 <> Tip s2 h2 b2 =
    Tip { slot = s1 + s2, height = h1 + h2, blockHash = b1 <> b2 }

instance Monoid Tip where
  mempty = Tip { slot = 0, height = 0, blockHash = Nothing }

cavefishApi :: Proxy CavefishApi
cavefishApi = Proxy

server :: Env -> Server CavefishApi
server env =
     healthH env
  :<|> infoH
  :<|> tipH
  :<|> utxoForAddressH

healthH :: Env -> Handler Health
healthH (Env t0) = do
  nowNs <- liftIO getMonotonicTimeNSec
  let secs = (toInteger nowNs - t0) `div` 1_000_000_000
  pure Health { status = 200, uptimeS = secs }

infoH :: Handler Info
infoH = liftIO (pure (Info Preprod) :: IO Info)

tipH :: Handler Tip
tipH = liftIO queryTip
  where
    queryTip :: IO Tip
    queryTip = do
      ct <- Api.getLocalChainTip connectInfo
      case ct of
        Api.ChainTip sl h (Api.BlockNo bn) ->
          pure Tip
            { slot      = sl
            , height    = fromIntegral bn
            , blockHash = Just (Api.serialiseToRawBytesHexText h)
            }
        Api.ChainTipAtGenesis ->
          pure Tip
            { slot      = Api.SlotNo 0
            , height    = 0
            , blockHash = Nothing
            }

utxoForAddressH :: Text -> Handler [(Api.TxIn, Api.TxOut Api.CtxUTxO Api.ConwayEra)]
utxoForAddressH addr = do
  utxo <- liftIO $ utxoForAddress connectInfo Api.ShelleyBasedEraConway addr
  case utxo of
    Left e -> error $ show e
    Right u -> pure (Map.toList (Api.unUTxO u))

parseAddressAny :: Text -> Handler Api.AddressAny
parseAddressAny t =
  case Api.deserialiseAddress Api.AsAddressAny t of
    Nothing -> throwError err400 { errBody = "Invalid address" }
    Just a  -> pure a

type Hex      = Text