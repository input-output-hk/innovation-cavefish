module Core.Api.ServerContext where

import Cardano.Api (
  ConwayEra,
  FromJSON,
  MonadError,
  MonadIO,
  ToJSON,
  Tx,
 )
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Core.Intent (IntentDSL, TxUnsigned)
import Data.Default (Default (def))
import GHC.Generics (Generic)
import Servant.Server (Handler)
import Servant.Server.Internal.ServerError (ServerError)
import Toml.Schema (FromValue (fromValue), parseTableFromValue, reqKey)
import WBPS.Commitment (Session)
import WBPS.Core.Keys.Ed25519 (PaymentAddess (..), UserWalletPublicKey)
import WBPS.Registration (AccountCreated)

data ServerContext = ServerContext
  { wbpsServices :: WBPSServices
  , txBuildingService :: TxBuildingService
  }

data WBPSServices = WBPSServices
  { register ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      UserWalletPublicKey ->
      m AccountCreated
  , createSession ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      UserWalletPublicKey -> Tx ConwayEra -> m Session
  , loadAccount ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      UserWalletPublicKey -> m (Maybe AccountCreated)
  , loadAccounts ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      m [AccountCreated]
  }

data TxBuildingService = TxBuildingService
  { fees :: ServiceFee
  , build :: forall m. (MonadIO m, MonadError ServerError m) => IntentDSL -> m TxUnsigned
  , submit :: forall m. (MonadIO m, MonadError ServerError m) => Tx ConwayEra -> m ()
  }

data ServiceFee = ServiceFee
  { amount :: Integer
  , paymentAddress :: PaymentAddess
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance FromValue ServiceFee where
  fromValue =
    parseTableFromValue
      (ServiceFee <$> reqKey "amount" <*> fmap PaymentAddess (reqKey "paymentAddress"))

instance Default ServiceFee where
  def = ServiceFee {amount = 0, paymentAddress = PaymentAddess ""}

newtype ServerM a = ServerM {unServerM :: ReaderT ServerContext Handler a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader ServerContext
    , MonadError ServerError
    )

runApp :: ServerContext -> ServerM a -> Handler a
runApp serverContext (ServerM m) = runReaderT m serverContext
