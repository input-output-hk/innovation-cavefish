module WBPS.Core.Registration.Registered (
  Registered (..),
) where

import WBPS.Core.Registration.Artefacts.Groth16.Setup (Setup)
import WBPS.Core.Registration.RegistrationId (RegistrationId)

data Registered
  = Registered
  { registrationId :: RegistrationId
  , setup :: Setup
  }
  deriving (Ord, Eq, Show)
