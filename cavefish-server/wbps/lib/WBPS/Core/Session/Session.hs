{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | Module for fetching and loading user accounts from the file system.
-- This module provides functions to load existing accounts, load a specific account,
-- and retrieve all recorded user wallet public keys. It handles errors related to
-- missing encryption keys and uses a file scheme for directory structure.
module WBPS.Core.Session.Session (
  Session (..),
) where

import GHC.Generics (Generic)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Commitment (Commitment (Commitment), id)
import WBPS.Core.Session.Steps.Demonstration.Demonstrated (CommitmentDemonstrated (CommitmentDemonstrated, commitment))
import WBPS.Core.Session.Steps.Demonstration.Persistence.Events qualified as Demonstrated
import WBPS.Core.Session.Steps.Proving.Persistence.Events qualified as Proved
import WBPS.Core.Session.Steps.Submitting.Persistence.Events qualified as Submitted

data Session
  = Demonstrated Demonstrated.EventHistory
  | Proved Proved.EventHistory
  | Submitted Submitted.EventHistory
  deriving (Eq, Show, Generic)

instance Ord Session where
  compare a b = commitmentIdFromSession a `compare` commitmentIdFromSession b
    where
      commitmentIdFromSession
        (Demonstrated Demonstrated.EventHistory {demonstrated = CommitmentDemonstrated {commitment = Commitment {id = sessionCommitmentId}}}) =
          sessionCommitmentId
      commitmentIdFromSession
        (Proved Proved.EventHistory {demonstrated = CommitmentDemonstrated {commitment = Commitment {id = sessionCommitmentId}}}) =
          sessionCommitmentId
      commitmentIdFromSession
        (Submitted Submitted.EventHistory {demonstrated = CommitmentDemonstrated {commitment = Commitment {id = sessionCommitmentId}}}) =
          sessionCommitmentId
