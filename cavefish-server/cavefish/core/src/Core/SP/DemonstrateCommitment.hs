{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Core.SP.DemonstrateCommitment (
  handle,
  Inputs (..),
  Outputs (..),
  Commitment (..),
) where

import Cardano.Api (ConwayEra, Tx)
import Cardano.Api qualified as Api
import Control.Monad.Reader (MonadReader (ask))
import Core.Api.ServerContext (
  ServerContext (ServerContext, txBuildingService, wbpsServices),
  ServerM,
  TxBuildingService (TxBuildingService, build),
  WBPSServices (WBPSServices, createSession),
 )
import Core.Intent (
  IntentDSL,
  TxUnsigned (TxUnsigned),
 )
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
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

handle :: Inputs -> ServerM Outputs
handle Inputs {userWalletPublicKey, intent} = do
  ServerContext
    { txBuildingService = TxBuildingService {build}
    , wbpsServices = WBPSServices {createSession}
    } <-
    ask

  TxUnsigned tx <- build intent

  let unsignedTx = Api.makeSignedTransaction [] tx
  SessionCreated {publicMessage = PublicMessage txAbs, commitment} <-
    createSession userWalletPublicKey unsignedTx
  return Outputs {txAbs, commitment}
