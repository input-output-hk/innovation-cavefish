-- | Module for handling the demonstration of a commitment in the Cavefish server.
-- This module defines the inputs and outputs for the demonstration process,
-- and provides a handler function that builds an unsigned transaction based on
-- the provided intent, creates a session for the commitment, and returns the
-- generated commitment along with the abstract unsigned transaction.
module Cavefish.Endpoints.Write.Session.Demonstrate (
  handle,
  Inputs (..),
  Outputs (..),
) where

import Cavefish (
  CavefishServerM,
  CavefishServices (CavefishServices, txBuildingService, wbpsService),
 )
import Cavefish.Services.TxBuilding qualified as TxService
import Cavefish.Services.WBPS qualified as WbpsService
import Control.Monad.Reader (MonadReader (ask))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Intent.Example.DSL (IntentDSL)
import WBPS.Core.Registration.RegistrationId (RegistrationId)
import WBPS.Core.Session.SessionId (SessionId)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Cardano.UnsignedTx (AbstractUnsignedTx)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Commitment (Commitment)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.PreparedMessage (
  MessageParts (MessageParts, public),
  PreparedMessage (PreparedMessage, parts),
  PublicMessage (PublicMessage),
 )
import WBPS.Core.Session.Steps.Demonstration.Demonstrated (
  CommitmentDemonstrated (CommitmentDemonstrated, commitment, preparedMessage),
 )

-- | Inputs for demonstrating a commitment.
data Inputs = Inputs
  { registrationId :: RegistrationId
  -- ^ The user's wallet public key.
  , intent :: IntentDSL
  -- ^ The intent for which to demonstrate the commitment.
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | Outputs from demonstrating a commitment.
data Outputs = Outputs
  { sessionId :: SessionId
  , commitment :: Commitment
  -- ^ The generated commitment.
  , txAbs :: AbstractUnsignedTx
  -- ^ The abstract unsigned transaction.
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | Handle the demonstration of a commitment.
-- This function builds an unsigned transaction based on the provided intent,
-- creates a session for the commitment using the user's wallet public key,
-- and returns the abstract unsigned transaction along with the generated commitment.
handle :: Inputs -> CavefishServerM Outputs
handle Inputs {registrationId, intent} = do
  CavefishServices
    { txBuildingService = TxService.TxBuilding {build}
    , wbpsService = WbpsService.WBPS {demonstrate}
    } <-
    ask
  toOuput <$> (demonstrate registrationId =<< build intent)

toOuput :: (SessionId, CommitmentDemonstrated) -> Outputs
toOuput (sessionId, CommitmentDemonstrated {preparedMessage = PreparedMessage {parts = MessageParts {public = PublicMessage txAbs}}, commitment}) =
  Outputs {sessionId, txAbs, commitment}
