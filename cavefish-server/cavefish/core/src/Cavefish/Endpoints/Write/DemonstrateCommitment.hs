module Cavefish.Endpoints.Write.DemonstrateCommitment (
  handle,
  Inputs (..),
  Outputs (..),
  Commitment (..),
) where

import Cavefish (
  CavefishServerM,
  CavefishServices (CavefishServices, txBuildingService, wbpsService),
 )
import Cavefish.Services.TxBuilding qualified as TxService (TxBuilding (..))
import Cavefish.Services.WBPS qualified as WbpsService (WBPS (..))
import Control.Monad.Reader (MonadReader (ask))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Intent.Example.DSL (IntentDSL)
import WBPS.Commitment (
  Commitment (Commitment),
  PublicMessage (PublicMessage),
  Session (SessionCreated, commitment, publicMessage),
 )
import WBPS.Core.Cardano.UnsignedTx (AbstractUnsignedTx)
import WBPS.Core.Keys.Ed25519 (UserWalletPublicKey)

data Inputs = Inputs
  { userWalletPublicKey :: UserWalletPublicKey
  , intent :: IntentDSL
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data Outputs = Outputs
  { commitment :: Commitment
  , txAbs :: AbstractUnsignedTx
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

handle :: Inputs -> CavefishServerM Outputs
handle Inputs {userWalletPublicKey, intent} = do
  CavefishServices
    { txBuildingService = TxService.TxBuilding {build}
    , wbpsService = WbpsService.WBPS {createSession}
    } <-
    ask

  unsignedTx <- build intent
  SessionCreated {publicMessage = PublicMessage txAbs, commitment} <- createSession userWalletPublicKey unsignedTx
  return Outputs {txAbs, commitment}
