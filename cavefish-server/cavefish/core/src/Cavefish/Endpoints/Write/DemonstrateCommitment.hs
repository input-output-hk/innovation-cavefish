-- | Module for handling the demonstration of a commitment in the Cavefish server.
-- This module defines the inputs and outputs for the demonstration process,
-- and provides a handler function that builds an unsigned transaction based on
-- the provided intent, creates a session for the commitment, and returns the
-- generated commitment along with the abstract unsigned transaction.
module Cavefish.Endpoints.Write.DemonstrateCommitment (
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
import WBPS.Core.Cardano.UnsignedTx (AbstractUnsignedTx)
import WBPS.Core.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Session.Commitment (Commitment)
import WBPS.Core.Session.Create (Session (SessionCreated, commitment, publicMessage))
import WBPS.Core.ZK.Message (PublicMessage (PublicMessage))

-- | Inputs for demonstrating a commitment.
data Inputs = Inputs
  { userWalletPublicKey :: UserWalletPublicKey
  -- ^ The user's wallet public key.
  , intent :: IntentDSL
  -- ^ The intent for which to demonstrate the commitment.
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | Outputs from demonstrating a commitment.
data Outputs = Outputs
  { commitment :: Commitment
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
handle Inputs {userWalletPublicKey, intent} = do
  CavefishServices
    { txBuildingService = TxService.TxBuilding {build}
    , wbpsService = WbpsService.WBPS {create}
    } <-
    ask

  unsignedTx <- build intent
  SessionCreated {publicMessage = PublicMessage txAbs, commitment} <- create userWalletPublicKey unsignedTx
  return Outputs {txAbs, commitment}
