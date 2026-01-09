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
import GHC.Generics (Generic)
import WBPS.Core.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Session.Demonstration.Commitment (CommitmentId)
import WBPS.Core.Session.Demonstration.R (R)
import WBPS.Core.Session.Proving.Challenge (Challenge)
import WBPS.Core.Session.Proving.Proof (Proof)
import WBPS.Core.Session.Proving.Proved (CommitmentProved (CommitmentProved, challenge, proof))

data Inputs = Inputs
  { userWalletPublicKey :: UserWalletPublicKey
  , commitmentId :: CommitmentId
  , bigR :: R
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data Outputs = Outputs
  { challenge :: Challenge
  , proof :: Proof
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

handle :: Inputs -> CavefishServerM Outputs
handle Inputs {userWalletPublicKey, commitmentId, bigR} = do
  CavefishServices {wbpsService = WbpsService.WBPS {prove}} <- ask
  CommitmentProved {..} <- prove userWalletPublicKey commitmentId bigR
  pure Outputs {challenge, proof = proof}
