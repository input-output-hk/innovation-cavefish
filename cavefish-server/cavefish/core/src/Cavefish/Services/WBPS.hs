module Cavefish.Services.WBPS (
  WBPS (..),
) where

import Cardano.Api (
  ConwayEra,
  MonadError,
  MonadIO,
  Tx,
 )
import Servant.Server.Internal.ServerError (ServerError)
import WBPS.Commitment (Session)
import WBPS.Core.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Registration (AccountCreated)

data WBPS = WBPS
  { register ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      UserWalletPublicKey ->
      m AccountCreated
  , createSession ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      UserWalletPublicKey -> Tx ConwayEra -> m Session
  , loadAccount ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      UserWalletPublicKey -> m (Maybe AccountCreated)
  , loadAccounts ::
      forall m.
      (MonadIO m, MonadError ServerError m) =>
      m [AccountCreated]
  }
