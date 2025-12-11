{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Core.SP.FetchAccount (handle, Inputs (..), Outputs (..), Account (..)) where

import Control.Monad.Reader (MonadReader (ask))
import Core.Api.ServerContext (
  ServerContext (..),
  ServerM,
  WBPSServices (..),
 )
import Data.Aeson
import GHC.Generics (Generic)
import WBPS.Core.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Keys.ElGamal qualified as ElGamal
import WBPS.Registration (AccountCreated (..), asJson)

handle :: Inputs -> ServerM Outputs
handle Inputs {userWalletPublicKey} = do
  ServerContext {wbpsServices = WBPSServices {loadAccount}} <- ask
  loadAccount userWalletPublicKey
    >>= \case
      Nothing -> pure . Outputs $ Nothing
      (Just AccountCreated {encryptionKeys = ElGamal.KeyPair {..}, publicVerificationContext}) ->
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
