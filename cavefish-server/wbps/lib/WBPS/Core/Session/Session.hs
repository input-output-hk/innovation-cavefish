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
import WBPS.Core.Keys.ElGamal (
  Rho,
 )
import WBPS.Core.Registration.Account (AccountCreated)
import WBPS.Core.Session.Challenge (Challenge)
import WBPS.Core.Session.Commitment (Commitment (Commitment), CommitmentId (CommitmentId), id)
import WBPS.Core.Session.Commitment.Scalars as CommitmentScalars (CommitmentScalars)
import WBPS.Core.Session.R (R)
import WBPS.Core.ZK.Message (Message, PublicMessage)

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
  { message :: Message
  , publicMessage :: PublicMessage
  , rho :: Rho
  , commitmentScalars :: CommitmentScalars
  , commitment :: Commitment
  }
  deriving (Eq, Show, Generic)

data CommitmentProved
  = CommitmentProved
  { bigR :: R
  , challenge :: Challenge
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
