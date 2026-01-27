module Cavefish.Services.WBPS (
  WBPS (..),
) where

import Cardano.Api (
  MonadError,
  MonadIO,
 )
import Servant.Server.Internal.ServerError (ServerError)
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Registration.Registered (Registered)
import WBPS.Core.Registration.RegistrationId (RegistrationId)
import WBPS.Core.Session.Session (Session)
import WBPS.Core.Session.SessionId (SessionId)
import WBPS.Core.Session.Steps.BlindSigning.BlindSignature (BlindSignature)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Cardano.UnsignedTx (UnsignedTx)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.R (R)
import WBPS.Core.Session.Steps.Demonstration.Demonstrated (CommitmentDemonstrated)
import WBPS.Core.Session.Steps.Proving.Proved (CommitmentProved)
import WBPS.Core.Session.Steps.Submitting.Artefacts.SubmittedTx (SubmitTx)
import WBPS.Core.Session.Steps.Submitting.Submitted (CommitmentSubmitted)

data WBPS = WBPS
  { register ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      UserWalletPublicKey ->
      m Registered
  , demonstrate ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      RegistrationId -> UnsignedTx -> m (SessionId, CommitmentDemonstrated)
  , prove ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      SessionId -> R -> m CommitmentProved
  , submit ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      SessionId -> SubmitTx m -> BlindSignature -> m CommitmentSubmitted
  , loadRegisteredMaybe ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      RegistrationId -> m (Maybe Registered)
  , loadAllRegistered ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      m [Registered]
  , loadSession ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      SessionId -> m Session
  , loadCommitmentDemonstrationEvents ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      SessionId -> m (Registered, CommitmentDemonstrated)
  }
