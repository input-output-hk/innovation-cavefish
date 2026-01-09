{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | Module for fetching and loading user accounts from the file system.
-- This module provides functions to load existing accounts, load a specific account,
-- and retrieve all recorded user wallet public keys. It handles errors related to
-- missing encryption keys and uses a file scheme for directory structure.
module WBPS.Core.Session.Session (
  SessionId (..),
  Session (..),
  deriveId,
) where

import Data.Text qualified as Text
import GHC.Generics (Generic)
import WBPS.Adapter.CardanoCryptoClass.Crypto (Codec (encode))
import WBPS.Core.Registration.Registered (Registered)
import WBPS.Core.Session.Demonstration.Commitment (Commitment (Commitment), CommitmentId (CommitmentId), id)
import WBPS.Core.Session.Demonstration.Demonstrated (CommitmentDemonstrated (CommitmentDemonstrated, commitment))
import WBPS.Core.Session.Proving.Proved (CommitmentProved)

newtype SessionId = SessionId String deriving (Show, Eq)

deriveId :: CommitmentId -> SessionId
deriveId (CommitmentId x) = SessionId . Text.unpack . encode $ x

data Session
  = Demonstrated
      { registered :: Registered
      , demonstrated :: CommitmentDemonstrated
      }
  | Proved
      { registered :: Registered
      , demonstrated :: CommitmentDemonstrated
      , proved :: CommitmentProved
      }
  deriving (Eq, Show, Generic)

instance Ord Session where
  compare a b = commitmentIdFromSession a `compare` commitmentIdFromSession b
    where
      commitmentIdFromSession
        Demonstrated {demonstrated = CommitmentDemonstrated {commitment = Commitment {id = sessionCommitmentId}}} =
          sessionCommitmentId
      commitmentIdFromSession
        Proved {demonstrated = CommitmentDemonstrated {commitment = Commitment {id = sessionCommitmentId}}} =
          sessionCommitmentId
