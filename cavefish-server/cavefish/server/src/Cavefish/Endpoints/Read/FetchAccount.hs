module Cavefish.Endpoints.Read.FetchAccount (handle, Inputs (..), Outputs (..), Account (..)) where

import Cavefish (
  CavefishServerM,
  CavefishServices (CavefishServices, wbpsService),
 )
import Cavefish.Services.WBPS qualified as Service (WBPS (WBPS, loadAccount))
import Control.Monad.Reader (MonadReader (ask))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import WBPS.Core.Registration.Artefacts.Groth16.Setup (
  PublicVerificationContextAsJSON,
  Setup (Setup, encryptionKeys, publicVerificationContext),
  asJson,
 )
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Registration.Artefacts.Keys.ElGamal qualified as ElGamal (EncryptionKey, KeyPair (KeyPair, ek))
import WBPS.Core.Registration.Registered (Registered (Registered, setup))

handle :: Inputs -> CavefishServerM Outputs
handle Inputs {userWalletPublicKey} = do
  CavefishServices {wbpsService = Service.WBPS {loadAccount}} <- ask
  loadAccount userWalletPublicKey
    >>= \case
      Nothing -> pure . Outputs $ Nothing
      (Just Registered {setup = Setup {encryptionKeys = ElGamal.KeyPair {..}, publicVerificationContext}}) ->
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
  , publicVerificationContext :: PublicVerificationContextAsJSON
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
