module Core.Api.ServerContext where

import Cardano.Api (
  AddressInEra,
  ConwayEra,
  FromJSON,
  MonadError,
  MonadIO,
  ToJSON,
  Tx,
 )
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Cooked.Wallet (Wallet, knownWallets)
import Core.Intent (IntentDSL, TxUnsigned)
import Data.Default (Default (def))
import Data.Map.Strict qualified as Map
import GHC.Generics (Generic)
import Ledger.Address qualified as Ledger
import Ledger.CardanoWallet qualified as CW
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

resolveWalletFromList ::
  [Wallet] ->
  AddressInEra ConwayEra ->
  Maybe Wallet
resolveWalletFromList wallets =
  let walletIndex =
        Map.fromList
          [ (Ledger.unPaymentPubKeyHash (CW.paymentPubKeyHash wallet), wallet)
          | wallet <- wallets
          ]
   in \addr -> do
        pkh <- Ledger.cardanoPubKeyHash addr
        Map.lookup pkh walletIndex

defaultWalletResolver :: AddressInEra ConwayEra -> Maybe Wallet
defaultWalletResolver = resolveWalletFromList knownWallets
