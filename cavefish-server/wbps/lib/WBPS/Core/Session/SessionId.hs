module WBPS.Core.Session.SessionId (
  SessionId (..),
  toString,
) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import WBPS.Adapter.CardanoCryptoClass.Crypto (Codec (encode))
import WBPS.Core.Registration.RegistrationId (RegistrationId)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Commitment (CommitmentId (CommitmentId))

data SessionId = SessionId
  { registrationId :: RegistrationId
  , commitmentId :: CommitmentId
  }
  deriving (Show, Eq, Generic, FromJSON, ToJSON)

toString :: SessionId -> String
toString (SessionId _ (CommitmentId x)) = Text.unpack (encode x)
