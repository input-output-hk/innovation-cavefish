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
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Session.Demonstration.Artefacts.Commitment (CommitmentId)
import WBPS.Core.Session.Demonstration.Artefacts.R (R)
import WBPS.Core.Session.Proving.Artefacts.Challenge (Challenge)
import WBPS.Core.Session.Proving.Artefacts.Proof (Proof)
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
  toOutputs <$> prove userWalletPublicKey commitmentId bigR

toOutputs :: CommitmentProved -> Outputs
toOutputs CommitmentProved {challenge = provedChallenge, proof = provedProof} =
  Outputs {challenge = provedChallenge, proof = provedProof}
