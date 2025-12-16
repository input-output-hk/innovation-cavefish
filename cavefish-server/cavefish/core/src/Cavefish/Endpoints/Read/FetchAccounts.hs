module Cavefish.Endpoints.Read.FetchAccounts (handle, Outputs (..), Account (..)) where

import Cavefish (
  CavefishServerM,
  CavefishServices (CavefishServices, wbpsService),
 )
import Cavefish.Services.WBPS qualified as Service (WBPS (..))
import Control.Monad.Reader (MonadReader (ask))
import Data.Aeson (FromJSON, ToJSON, Value)
import GHC.Generics (Generic)
import WBPS.Core.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Keys.ElGamal qualified as ElGamal (EncryptionKey, KeyPair (KeyPair, ek))
import WBPS.Registration (
  AccountCreated (
    AccountCreated,
    encryptionKeys,
    provingKey,
    publicVerificationContext,
    userWalletPublicKey
  ),
  PublicVerificationContext (asJson),
 )

handle :: CavefishServerM Outputs
handle = do
  CavefishServices {wbpsService = Service.WBPS {loadAccounts}} <- ask
  accountsCreated <- loadAccounts
  return
    . Outputs
    . map
      ( \AccountCreated {encryptionKeys = ElGamal.KeyPair {..}, ..} ->
          Account {publicVerificationContext = asJson publicVerificationContext, ..}
      )
    $ accountsCreated

newtype Outputs = Outputs
  { accounts :: [Account]
  }
  deriving newtype (Eq, Show, ToJSON, FromJSON)

data Account = Account
  { userWalletPublicKey :: UserWalletPublicKey
  , ek :: ElGamal.EncryptionKey
  , publicVerificationContext :: Value
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
