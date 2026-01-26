module Cavefish.Endpoints.Read.FetchAccount (handle, Inputs (..), Outputs (..), Account (..)) where

import Cavefish (
  CavefishServerM,
  CavefishServices (CavefishServices, wbpsService),
 )
import Cavefish.Services.WBPS qualified as Service (WBPS (WBPS, loadRegisteredMaybe))
import Control.Monad.Reader (MonadReader (ask))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import WBPS.Core.Registration.Artefacts.Groth16.Setup (
  PublicVerificationContextAsJSON,
  Setup (Setup, encryptionKeys, publicVerificationContext),
  asJson,
 )
import WBPS.Core.Registration.Artefacts.Keys.ElGamal qualified as ElGamal (EncryptionKey, KeyPair (KeyPair, ek))
import WBPS.Core.Registration.Registered (Registered (Registered, registrationId, setup))
import WBPS.Core.Registration.RegistrationId (RegistrationId)

handle :: Inputs -> CavefishServerM Outputs
handle Inputs {registrationId} = do
  CavefishServices {wbpsService = Service.WBPS {loadRegisteredMaybe}} <- ask
  toOutputs <$> loadRegisteredMaybe registrationId

toOutputs :: Maybe Registered -> Outputs
toOutputs Nothing = Outputs Nothing
toOutputs (Just Registered {registrationId, setup = Setup {encryptionKeys = ElGamal.KeyPair {..}, publicVerificationContext}}) =
  Outputs . Just $ Account {publicVerificationContext = asJson publicVerificationContext, ..}

newtype Inputs = Inputs
  { registrationId :: RegistrationId
  }
  deriving newtype (Eq, Show, ToJSON, FromJSON)

newtype Outputs = Outputs
  { accountMaybe :: Maybe Account
  }
  deriving newtype (Eq, Show, ToJSON, FromJSON)

data Account = Account
  { registrationId :: RegistrationId
  , ek :: ElGamal.EncryptionKey
  , publicVerificationContext :: PublicVerificationContextAsJSON
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
