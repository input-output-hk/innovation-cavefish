{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | Module for fetching and loading user accounts from the file system.
-- This module provides functions to load existing accounts, load a specific account,
-- and retrieve all recorded user wallet public keys. It handles errors related to
-- missing encryption keys and uses a file scheme for directory structure.
module WBPS.Core.Session.Session (
  SessionId (..),
  Session (..),
  CommitmentDemonstrated (..),
  CommitmentProved (..),
  deriveId,
) where

import Data.Text qualified as Text
import GHC.Generics (Generic)
import WBPS.Adapter.CardanoCryptoClass.Crypto (Codec (encode))
import WBPS.Core.Registration.Account (AccountCreated)
import WBPS.Core.Session.Demonstration.Commitment (Commitment (Commitment), CommitmentId (CommitmentId), id)
import WBPS.Core.Session.Demonstration.Message (PreparedMessage)
import WBPS.Core.Session.Demonstration.R (R)
import WBPS.Core.Session.Demonstration.Scalars (Scalars)
import WBPS.Core.Session.Proving.Challenge (Challenge)
import WBPS.Core.Session.Proving.Proof (Proof)

newtype SessionId = SessionId String deriving (Show, Eq)

deriveId :: CommitmentId -> SessionId
deriveId (CommitmentId x) = SessionId . Text.unpack . encode $ x

data Session
  = SessionCreated
      { account :: AccountCreated
      , commitmentDemonstrated :: CommitmentDemonstrated
      }
  | SessionWithProof
      { account :: AccountCreated
      , commitmentDemonstrated :: CommitmentDemonstrated
      , commitmentProved :: CommitmentProved
      }
  deriving (Eq, Show, Generic)

data CommitmentDemonstrated
  = CommitmentDemonstrated
  { preparedMessage :: PreparedMessage
  , scalars :: Scalars
  , commitment :: Commitment
  }
  deriving (Eq, Show, Generic)

data CommitmentProved
  = CommitmentProved
  { bigR :: R
  , challenge :: Challenge
  , proof :: Proof
  }
  deriving (Eq, Show, Generic)

instance Ord Session where
  compare a b = commitmentIdFromSession a `compare` commitmentIdFromSession b
    where
      commitmentIdFromSession
        SessionCreated {commitmentDemonstrated = CommitmentDemonstrated {commitment = Commitment {id = sessionCommitmentId}}} =
          sessionCommitmentId
      commitmentIdFromSession
        SessionWithProof {commitmentDemonstrated = CommitmentDemonstrated {commitment = Commitment {id = sessionCommitmentId}}} =
          sessionCommitmentId
