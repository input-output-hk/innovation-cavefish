{-# LANGUAGE QuasiQuotes #-}

module WBPS.Core.Session.Steps.Demonstration.Persistence.Events (
  EventHistory (..),
  loadHistory,
  load,
  persist,
) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader.Class (asks)
import Path (Dir, Path, reldir, (</>))
import Path.IO (ensureDir)
import WBPS.Adapter.Monad.Control (whenNothingThrow)
import WBPS.Adapter.Path (readFrom, writeTo)
import WBPS.Core.Failure (WBPSFailure (SessionCommitmentNotFound, SessionPreparedMessageNotFound, SessionScalarsNotFound))
import WBPS.Core.Registration.FetchAccounts (loadRegistered)
import WBPS.Core.Registration.Registered (Registered (Registered, registrationId))
import WBPS.Core.Session.Persistence.FileScheme (deriveExistingSessionDirectoryFrom, deriveSessionDirectoryFrom)
import WBPS.Core.Session.SessionId (SessionId (SessionId, registrationId))
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Commitment (Commitment (Commitment, id))
import WBPS.Core.Session.Steps.Demonstration.Demonstrated (
  CommitmentDemonstrated (
    CommitmentDemonstrated,
    commitment,
    preparedMessage,
    scalars
  ),
 )
import WBPS.Core.Setup.Circuit.FileScheme (FileScheme)
import WBPS.Core.Setup.Circuit.FileScheme qualified as FileScheme

data EventHistory = EventHistory
  { registered :: Registered
  , demonstrated :: CommitmentDemonstrated
  }
  deriving (Eq, Show)

loadHistory ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  SessionId -> m EventHistory
loadHistory sessionId@SessionId {registrationId} = do
  sessionDirectory <- deriveExistingSessionDirectoryFrom sessionId
  EventHistory
    <$> loadRegistered registrationId
    <*> load sessionDirectory sessionId

load ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  Path b Dir ->
  SessionId ->
  m CommitmentDemonstrated
load sessionDirectory sessionId = do
  demonstration <- asks (FileScheme.demonstration . FileScheme.session . FileScheme.account)
  let demonstratedDirectory = sessionDirectory </> [reldir|demonstrated|]
  CommitmentDemonstrated
    <$> ( readFrom (demonstratedDirectory </> FileScheme.preparedMessage demonstration)
            >>= whenNothingThrow [SessionPreparedMessageNotFound sessionId]
        )
    <*> ( readFrom (demonstratedDirectory </> FileScheme.scalars demonstration)
            >>= whenNothingThrow [SessionScalarsNotFound sessionId]
        )
    <*> ( readFrom (demonstratedDirectory </> FileScheme.commitment demonstration)
            >>= whenNothingThrow [SessionCommitmentNotFound sessionId]
        )

persist ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  Registered -> CommitmentDemonstrated -> m CommitmentDemonstrated
persist
  Registered {registrationId}
  event@CommitmentDemonstrated
    { preparedMessage
    , scalars
    , commitment = commitment@Commitment {id = commitmentId}
    } = do
    sessionDirectory <- deriveSessionDirectoryFrom (SessionId registrationId commitmentId)
    ensureDir sessionDirectory
    demonstration <- asks (FileScheme.demonstration . FileScheme.session . FileScheme.account)
    writeTo (sessionDirectory </> [reldir|demonstrated|] </> FileScheme.preparedMessage demonstration) preparedMessage
    writeTo (sessionDirectory </> [reldir|demonstrated|] </> FileScheme.scalars demonstration) scalars
    writeTo (sessionDirectory </> [reldir|demonstrated|] </> FileScheme.commitment demonstration) commitment
    return event
