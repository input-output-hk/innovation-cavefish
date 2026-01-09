module Cavefish.Services.WBPS (
  WBPS (..),
) where

import Cardano.Api (
  MonadError,
  MonadIO,
 )
import Servant.Server.Internal.ServerError (ServerError)
import WBPS.Core.Cardano.UnsignedTx (UnsignedTx)
import WBPS.Core.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Registration.Account (AccountCreated)
import WBPS.Core.Session.Challenge (Challenge)
import WBPS.Core.Session.Commitment (CommitmentId)
import WBPS.Core.Session.Proof (Proof)
import WBPS.Core.Session.R (R)
import WBPS.Core.Session.Session (CommitmentDemonstrated, Session)

data WBPS = WBPS
  { register ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      UserWalletPublicKey ->
      m AccountCreated
  , demonstrate ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      UserWalletPublicKey -> UnsignedTx -> m Session
  , prove ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      UserWalletPublicKey -> CommitmentId -> R -> m (Challenge, Proof)
  , loadAccount ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      UserWalletPublicKey -> m (Maybe AccountCreated)
  , loadAccounts ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      m [AccountCreated]
  , loadSession ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      UserWalletPublicKey -> CommitmentId -> m Session
  , loadCommitmentDemonstrationEvents ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      UserWalletPublicKey -> CommitmentId -> m (AccountCreated, CommitmentDemonstrated)
  }
