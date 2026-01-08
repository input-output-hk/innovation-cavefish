{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Cavefish.Endpoints.Write.AskCommitmentProof (handle, Inputs (..), Outputs (..)) where

import Cavefish (
  CavefishServerM,
  CavefishServices (CavefishServices, wbpsService),
 )
import Cavefish.Services.WBPS qualified as WbpsService
import Control.Monad.Reader (ask)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import WBPS.Core.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Session.Challenge (Challenge)
import WBPS.Core.Session.Challenge qualified as Challenge
import WBPS.Core.Session.Commitment (CommitmentId)
import WBPS.Core.Session.R (R)
import WBPS.Core.Session.Session (CommitmentDemonstrated (CommitmentDemonstrated, message))

data Inputs = Inputs
  { userWalletPublicKey :: UserWalletPublicKey
  , commitmentId :: CommitmentId
  , bigR :: R
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data Outputs = Outputs
  { challenge :: Challenge
  , proof :: Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

handle :: Inputs -> CavefishServerM Outputs
handle Inputs {userWalletPublicKey, commitmentId, bigR} = do
  CavefishServices {wbpsService = WbpsService.WBPS {loadCommitmentDemonstrationEvents}} <- ask
  (_, CommitmentDemonstrated {message}) <-
    loadCommitmentDemonstrationEvents userWalletPublicKey commitmentId
  let challenge = Challenge.computeByUsingTxId userWalletPublicKey message bigR
  -- generateWitness accountCreated commitmentDemonstrated bigR challenge

  pure Outputs {proof = "proof", challenge}
