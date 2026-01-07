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
import WBPS.Core.Session.Commitment (CommitmentId)
import WBPS.Core.Session.Create (Session)

data WBPS = WBPS
  { register ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      UserWalletPublicKey ->
      m AccountCreated
  , createSession ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      UserWalletPublicKey -> UnsignedTx -> m Session
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
  }
