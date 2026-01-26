module Cavefish.Endpoints.Read.FetchTxStatus (handle, Inputs (..), Outputs (..)) where

import Cardano.Api (TxId)
import Cavefish (
  CavefishServerM,
  CavefishServices (CavefishServices, txBuildingService),
 )
import Cavefish.Services.TxBuilding qualified as TxService
import Control.Monad.Reader (MonadReader (ask))
import Data.Aeson (FromJSON, ToJSON)

newtype Inputs = Inputs
  { txId :: TxId
  }
  deriving newtype (Eq, Show, ToJSON, FromJSON)

newtype Outputs = Outputs
  { status :: TxService.TxStatus
  }
  deriving newtype (Eq, Show, ToJSON, FromJSON)

handle :: Inputs -> CavefishServerM Outputs
handle Inputs {txId} = do
  CavefishServices {txBuildingService = TxService.TxBuilding {txStatus}} <- ask
  Outputs <$> txStatus txId
