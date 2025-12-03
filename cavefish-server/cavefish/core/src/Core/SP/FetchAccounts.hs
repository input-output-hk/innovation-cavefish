{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Core.SP.FetchAccounts (handle, Outputs (..), Account (..)) where

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

handle :: AppM Outputs
handle = do
  Env {wbpsScheme} <- ask
  liftIO (WBPS.withFileSchemeIO wbpsScheme WBPS.loadAccounts)
    >>= \case
      (Left e) -> throwError err500 {errBody = BL8.pack ("Unexpected event" ++ show e)}
      (Right accountsCreated) ->
        return
          . Outputs
          . map
            ( \WBPS.AccountCreated {encryptionKeys = ElGamal.KeyPair {..}, ..} ->
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
