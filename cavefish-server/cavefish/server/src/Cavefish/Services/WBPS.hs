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
import WBPS.Core.Session.Demonstration.Artefacts.Cardano.UnsignedTx (UnsignedTx)
import WBPS.Core.Session.Demonstration.Artefacts.Commitment (CommitmentId)
import WBPS.Core.Session.Demonstration.Artefacts.R (R)
import WBPS.Core.Session.Demonstration.Demonstrated (CommitmentDemonstrated)
import WBPS.Core.Session.Proving.Proved (CommitmentProved)
import WBPS.Core.Session.Session (Session)

data WBPS = WBPS
  { register ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      UserWalletPublicKey ->
      m Registered
  , demonstrate ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      UserWalletPublicKey -> UnsignedTx -> m Session
  , prove ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      UserWalletPublicKey -> CommitmentId -> R -> m CommitmentProved
  , loadAccount ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      UserWalletPublicKey -> m (Maybe Registered)
  , loadAccounts ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      m [Registered]
  , loadSession ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      UserWalletPublicKey -> CommitmentId -> m Session
  , loadCommitmentDemonstrationEvents ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      UserWalletPublicKey -> CommitmentId -> m (Registered, CommitmentDemonstrated)
  }
