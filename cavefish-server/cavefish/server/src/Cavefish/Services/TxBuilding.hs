module Cavefish.Services.TxBuilding (
  TxBuilding (..),
  ServiceFee (..),
  TxStatus (..),
) where

import Cardano.Api (
  ConwayEra,
  FromJSON,
  MonadError,
  MonadIO,
  ToJSON,
  Tx,
  TxId,
 )
import Data.Default (Default (def))
import GHC.Generics (Generic)
import Intent.Example.DSL (IntentDSL)
import Servant.Server.Internal.ServerError (ServerError)
import Toml.Schema (FromValue (fromValue), parseTableFromValue, reqKey)
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (PaymentAddess (PaymentAddess))
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Cardano.UnsignedTx (UnsignedTx)

data TxBuilding = TxBuilding
  { fees :: ServiceFee
  , build :: forall m. (MonadIO m, MonadError ServerError m) => IntentDSL -> m UnsignedTx
  , submit :: forall m. (MonadIO m, MonadError ServerError m) => Tx ConwayEra -> m ()
  , txStatus :: forall m. (MonadIO m, MonadError ServerError m) => TxId -> m TxStatus
  }

data ServiceFee = ServiceFee
  { amount :: Integer
  , paidTo :: PaymentAddess
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

data TxStatus
  = TxStatusUnknown
  | TxStatusSubmitted
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance FromValue ServiceFee where
  fromValue =
    parseTableFromValue
      (ServiceFee <$> reqKey "amount" <*> fmap PaymentAddess (reqKey "paymentAddress"))

instance Default ServiceFee where
  def = ServiceFee {amount = 0, paidTo = PaymentAddess ""}
