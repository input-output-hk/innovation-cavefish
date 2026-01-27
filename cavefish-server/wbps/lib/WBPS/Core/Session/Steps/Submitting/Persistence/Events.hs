{-# LANGUAGE QuasiQuotes #-}

module WBPS.Core.Session.Steps.Submitting.Persistence.Events (
  EventHistory (..),
  loadHistory,
  load,
  persist,
) where

import Cardano.Api qualified as Api
import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader.Class (asks)
import Path (Dir, Path, reldir, (</>))
import Path.IO (ensureDir)
import WBPS.Adapter.Monad.Control (whenNothingThrow)
import WBPS.Adapter.Path (readFrom, writeTo)
import WBPS.Core.Failure (WBPSFailure (SessionBlindSignatureNotFound, SessionSubmittedTxNotFound, SessionTxSignatureNotFound))
import WBPS.Core.Registration.FetchAccounts (loadRegistered)
import WBPS.Core.Registration.Registered (
  Registered (Registered, registrationId),
 )
import WBPS.Core.Session.Persistence.FileScheme (deriveExistingSessionDirectoryFrom, deriveSessionDirectoryFrom)
import WBPS.Core.Session.SessionId (SessionId (SessionId, commitmentId, registrationId))
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Commitment (Commitment (Commitment, id))
import WBPS.Core.Session.Steps.Demonstration.Demonstrated (CommitmentDemonstrated (CommitmentDemonstrated, commitment))
import WBPS.Core.Session.Steps.Demonstration.Persistence.Events qualified as Demonstrated
import WBPS.Core.Session.Steps.Proving.Persistence.Events qualified as Proved
import WBPS.Core.Session.Steps.Proving.Proved (CommitmentProved)
import WBPS.Core.Session.Steps.Submitting.Artefacts.SubmittedTx (SubmittedTx (SubmittedTx))
import WBPS.Core.Session.Steps.Submitting.Submitted (
  CommitmentSubmitted (CommitmentSubmitted, blindSignature, submittedTx, txId, txSignature),
 )
import WBPS.Core.Setup.Circuit.FileScheme (FileScheme)
import WBPS.Core.Setup.Circuit.FileScheme qualified as FileScheme

data EventHistory = EventHistory
  { registered :: Registered
  , demonstrated :: CommitmentDemonstrated
  , proved :: CommitmentProved
  , submitted :: CommitmentSubmitted
  }
  deriving (Eq, Show)

loadHistory ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  SessionId ->
  m EventHistory
loadHistory sessionId@SessionId {registrationId} = do
  sessionDirectory <- deriveExistingSessionDirectoryFrom sessionId
  EventHistory
    <$> loadRegistered registrationId
    <*> Demonstrated.load sessionDirectory sessionId
    <*> Proved.load sessionDirectory sessionId
    <*> load sessionDirectory sessionId

load ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  Path b Dir ->
  SessionId ->
  m CommitmentSubmitted
load sessionDirectory sessionId = do
  submitting <- asks (FileScheme.submitting . FileScheme.session . FileScheme.account)
  let submittedDirectory = sessionDirectory </> [reldir|submitted|]
  blindSignature <-
    readFrom (submittedDirectory </> FileScheme.blindSignature submitting)
      >>= whenNothingThrow [SessionBlindSignatureNotFound sessionId]
  txSignature <-
    readFrom (submittedDirectory </> FileScheme.txSignature submitting)
      >>= whenNothingThrow [SessionTxSignatureNotFound sessionId]
  submittedTx <-
    readFrom (submittedDirectory </> FileScheme.submittedTx submitting)
      >>= whenNothingThrow [SessionSubmittedTxNotFound sessionId]
  let SubmittedTx (Api.Tx txBody _) = submittedTx
  let txId = Api.getTxId txBody
  pure CommitmentSubmitted {blindSignature, txSignature, submittedTx, txId}

persist ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  Registered ->
  CommitmentDemonstrated ->
  CommitmentSubmitted ->
  m CommitmentSubmitted
persist
  Registered {registrationId}
  CommitmentDemonstrated {commitment = Commitment {id = commitmentId}}
  event@CommitmentSubmitted {blindSignature, txSignature, submittedTx} = do
    sessionDirectory <- deriveSessionDirectoryFrom (SessionId {..})
    ensureDir sessionDirectory
    submitting <- asks (FileScheme.submitting . FileScheme.session . FileScheme.account)
    let submittedDirectory = sessionDirectory </> [reldir|submitted|]
    writeTo (submittedDirectory </> FileScheme.blindSignature submitting) blindSignature
    writeTo (submittedDirectory </> FileScheme.txSignature submitting) txSignature
    writeTo (submittedDirectory </> FileScheme.submittedTx submitting) submittedTx
    return event
