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
import Data.Aeson (FromJSON, ToJSON, encode)
import Data.Text (Text)
import Data.Text.Lazy qualified as TextLazy
import Data.Text.Lazy.Encoding qualified as TextLazyEncoding
import GHC.Generics (Generic)
import WBPS.Core.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Session.Challenge (Challenge)
import WBPS.Core.Session.Commitment (CommitmentId)
import WBPS.Core.Session.Proof (Proof (Proof))
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
  , proof :: Proof
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

handle :: Inputs -> CavefishServerM Outputs
handle Inputs {userWalletPublicKey, commitmentId, bigR} = do
  CavefishServices {wbpsService = WbpsService.WBPS {prove}} <- ask
  (challenge, proof) <- prove userWalletPublicKey commitmentId bigR
  pure Outputs {challenge, proof = proof}
