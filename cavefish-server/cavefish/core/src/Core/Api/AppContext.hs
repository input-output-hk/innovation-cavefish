module Core.Api.AppContext where

import Cardano.Api qualified as Api
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Cooked (MockChainState)
import Cooked.Wallet (Wallet, knownWallets)
import Core.Api.State (CompleteStore, PendingStore)
import Core.Intent (BuildTxResult, Intent)
import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import Ledger.Address qualified as Ledger
import Ledger.CardanoWallet qualified as CW
import Servant.Server (Handler)
import Servant.Server.Internal.ServerError (ServerError)
import WBPS.Core.FileScheme (FileScheme)

data Env = Env
  { pending :: PendingStore
  , complete :: CompleteStore
  , ttl :: NominalDiffTime
  , resolveWallet :: Api.AddressInEra Api.ConwayEra -> Maybe Wallet
  , spFee :: Integer
  , wbpsScheme :: FileScheme
  , build ::
      Intent ->
      Maybe ByteString ->
      IO BuildTxResult
  , submit ::
      Api.Tx Api.ConwayEra ->
      MockChainState ->
      IO (Either Text ())
  }

newtype AppM a = AppM {unAppM :: ReaderT Env Handler a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader Env
    , MonadError ServerError
    )

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
