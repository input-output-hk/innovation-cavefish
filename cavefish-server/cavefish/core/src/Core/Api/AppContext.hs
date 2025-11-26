module Core.Api.AppContext where

import Blammo.Logging.Simple (
  HasLogger (loggerL),
  Logger,
  MonadLogger,
  MonadLoggerIO,
  WithLogger (WithLogger),
 )
import Cardano.Api qualified as Api
import Control.Lens (lens)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Cooked (MockChainState)
import Cooked.Wallet (Wallet, knownWallets)
import Core.Api.State (ClientRegistrationStore, CompleteStore, PendingStore)
import Core.Intent (BuildTxResult, Intent)
import Core.Pke (PkePublicKey, PkeSecretKey)
import Crypto.PubKey.Ed25519 (SecretKey)
import Data.Aeson (ToJSON)
import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import GHC.Generics (Generic)
import Ledger.Address qualified as Ledger
import Ledger.CardanoWallet qualified as CW
import Servant.Server (Handler)
import Servant.Server.Internal.ServerError (ServerError)
import WBPS.Core.FileScheme (FileScheme)

data HttpServerConfig = HttpServerConfig
  { host :: String
  , port :: Int
  }
  deriving (Show, Generic)

instance ToJSON HttpServerConfig

data Env = Env
  { spSk :: SecretKey
  , pending :: PendingStore
  , complete :: CompleteStore
  , clientRegistration :: ClientRegistrationStore
  , ttl :: NominalDiffTime
  , spWallet :: Wallet
  , resolveWallet :: Api.AddressInEra Api.ConwayEra -> Maybe Wallet
  , spFee :: Integer
  , pkeSecret :: PkeSecretKey
  , pkePublic :: PkePublicKey
  , wbpsScheme :: FileScheme
  , build ::
      Intent ->
      Maybe ByteString ->
      IO BuildTxResult
  , submit ::
      Api.Tx Api.ConwayEra ->
      MockChainState ->
      IO (Either Text ())
  , httpServerConfig :: HttpServerConfig
  , logger :: Logger
  }

instance HasLogger Env where
  loggerL = lens logger (\x y -> x {logger = y})

newtype AppM a = AppM {unAppM :: ReaderT Env Handler a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader Env
    , MonadError ServerError
    )
  deriving (MonadLogger, MonadLoggerIO) via (WithLogger Env Handler)

runApp :: Env -> AppM a -> Handler a
runApp env (AppM m) = runReaderT m env

resolveWalletFromList ::
  [Wallet] ->
  Api.AddressInEra Api.ConwayEra ->
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

defaultWalletResolver :: Api.AddressInEra Api.ConwayEra -> Maybe Wallet
defaultWalletResolver = resolveWalletFromList knownWallets
