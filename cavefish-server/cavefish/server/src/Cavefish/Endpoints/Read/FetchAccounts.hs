module Cavefish.Endpoints.Read.FetchAccounts (handle, Outputs (..), Account (..)) where

import Cavefish (
  CavefishServerM,
  CavefishServices (CavefishServices, wbpsService),
 )
import Cavefish.Services.WBPS qualified as Service (WBPS (WBPS, loadAllRegistered))
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
import WBPS.Core.Setup.Circuit.FileScheme ()

handle :: CavefishServerM Outputs
handle = do
  CavefishServices {wbpsService = Service.WBPS {loadAllRegistered}} <- ask
  toOuputs <$> loadAllRegistered

toOuputs :: [Registered] -> Outputs
toOuputs =
  Outputs
    . map
      ( \Registered {registrationId, setup = Setup {encryptionKeys = ElGamal.KeyPair {ek}, publicVerificationContext}} ->
          Account {publicVerificationContext = asJson publicVerificationContext, ..}
      )

newtype Outputs = Outputs
  { accounts :: [Account]
  }
  deriving newtype (Eq, Show, ToJSON, FromJSON)

data Account = Account
  { registrationId :: RegistrationId
  , ek :: ElGamal.EncryptionKey
  , publicVerificationContext :: PublicVerificationContextAsJSON
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)
