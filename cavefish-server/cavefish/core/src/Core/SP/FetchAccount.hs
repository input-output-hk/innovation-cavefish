{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Core.SP.FetchAccount (handle, Inputs (..), Outputs (..), Account (..)) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader (ask))
import Core.Api.AppContext (
  AppM,
  Env (
    Env,
    wbpsScheme
  ),
 )
import Data.Aeson
import Data.ByteString.Lazy.Char8 qualified as BL8
import GHC.Generics (Generic)
import Servant (
  err500,
  errBody,
  throwError,
 )
import WBPS (AccountCreated (..), asJson)
import WBPS qualified
import WBPS.Core.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Keys.ElGamal qualified as ElGamal

handle :: Inputs -> AppM Outputs
handle Inputs {userWalletPublicKey} = do
  Env {wbpsScheme} <- ask
  liftIO (WBPS.withFileSchemeIO wbpsScheme (WBPS.loadAccount userWalletPublicKey))
    >>= \case
      (Left e) -> throwError err500 {errBody = BL8.pack ("Unexpected event" ++ show e)}
      (Right Nothing) -> pure . Outputs $ Nothing
      (Right (Just AccountCreated {encryptionKeys = ElGamal.KeyPair {..}, publicVerificationContext})) ->
        pure . Outputs . Just $ Account {publicVerificationContext = asJson publicVerificationContext, ..}

newtype Inputs = Inputs
  { userWalletPublicKey :: UserWalletPublicKey
  }
  deriving newtype (Eq, Show, ToJSON, FromJSON)

newtype Outputs = Outputs
  { accountMaybe :: Maybe Account
  }
  deriving newtype (Eq, Show, ToJSON, FromJSON)

data Account = Account
  { userWalletPublicKey :: UserWalletPublicKey
  , ek :: ElGamal.EncryptionKey
  , publicVerificationContext :: Value
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
