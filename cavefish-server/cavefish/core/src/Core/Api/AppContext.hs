module Core.Api.AppContext where

import Cardano.Api qualified as Api
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Cooked (MockChainState)
import Cooked.Wallet (Wallet, knownWallets)
import Core.Api.State (ClientRegistrationStore, CompleteStore, PendingStore)
import Core.Intent (BuildTxResult, Intent)
import Core.Pke (PkePublicKey, PkeSecretKey)
import Crypto.PubKey.Ed25519 (SecretKey)
import Data.ByteString (ByteString)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import Ledger.Address qualified as Ledger
import Ledger.CardanoWallet qualified as CW
import Servant.Server (Handler)
import Servant.Server.Internal.ServerError (ServerError)

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
  , build ::
      Intent ->
      ByteString ->
      IO BuildTxResult
  , submit ::
      Api.Tx Api.ConwayEra ->
      MockChainState ->
      IO (Either Text ())
  }

newtype AppM a = AppM {unAppM :: ReaderT Env Handler a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadError ServerError)

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
