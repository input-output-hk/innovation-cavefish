{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Cavefish.Endpoints.Write.DemonstrateCommitment (
  handle,
  Inputs (..),
  Outputs (..),
  Commitment (..),
) where

import Cardano.Api (ConwayEra, Tx)
import Cardano.Api qualified as Api
import Cavefish (
  CavefishServerM,
  CavefishServices (CavefishServices, txBuildingService, wbpsService),
 )
import Cavefish.Services.TxBuilding qualified as Service
import Cavefish.Services.WBPS qualified as Service
import Control.Monad.Reader (MonadReader (ask))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Intent.Example.DSL (
  IntentDSL,
  TxUnsigned (TxUnsigned),
 )
import WBPS.Commitment (
  Commitment (..),
  PublicMessage (PublicMessage),
  Session (SessionCreated, commitment, publicMessage),
 )
import WBPS.Core.Keys.Ed25519 (UserWalletPublicKey)

data Inputs = Inputs
  { userWalletPublicKey :: UserWalletPublicKey
  , intent :: IntentDSL
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data Outputs = Outputs
  { commitment :: Commitment
  , txAbs :: Tx ConwayEra
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

handle :: Inputs -> CavefishServerM Outputs
handle Inputs {userWalletPublicKey, intent} = do
  CavefishServices
    { txBuildingService = Service.TxBuilding {build}
    , wbpsService = Service.WBPS {createSession}
    } <-
    ask

  TxUnsigned tx <- build intent

  let unsignedTx = Api.makeSignedTransaction [] tx
  SessionCreated {publicMessage = PublicMessage txAbs, commitment} <-
    createSession userWalletPublicKey unsignedTx
  return Outputs {txAbs, commitment}
