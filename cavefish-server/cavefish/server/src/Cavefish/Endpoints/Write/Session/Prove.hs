{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Cavefish.Endpoints.Write.Session.Prove (handle, Inputs (..), Outputs (..)) where

import Cavefish (
  CavefishServerM,
  CavefishServices (CavefishServices, wbpsService),
 )
import Cavefish.Services.WBPS qualified as WbpsService
import Control.Monad.Reader (ask)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import WBPS.Core.Session.SessionId (SessionId)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.R (R)
import WBPS.Core.Session.Steps.Proving.Artefacts.Challenge (Challenge)
import WBPS.Core.Session.Steps.Proving.Artefacts.Proof (Proof)
import WBPS.Core.Session.Steps.Proving.Proved (CommitmentProved (CommitmentProved, challenge, proof))

data Inputs = Inputs
  { sessionId :: SessionId
  , bigR :: R
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data Outputs = Outputs
  { challenge :: Challenge
  , proof :: Proof
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

handle :: Inputs -> CavefishServerM Outputs
handle Inputs {sessionId, bigR} = do
  CavefishServices {wbpsService = WbpsService.WBPS {prove}} <- ask
  toOutputs <$> prove sessionId bigR

toOutputs :: CommitmentProved -> Outputs
toOutputs CommitmentProved {challenge = provedChallenge, proof = provedProof} =
  Outputs {challenge = provedChallenge, proof = provedProof}
