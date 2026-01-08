{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module WBPS.Core.Session.Demonstrate (
  demonstrate,
) where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader.Class (asks)
import Path (reldir, (</>))
import Path.IO (ensureDir)
import WBPS.Adapter.Path (writeTo)
import WBPS.Core.Cardano.UnsignedTx (UnsignedTx)
import WBPS.Core.Failure (
  RegistrationFailed (AccountNotFound),
  toWBPSFailure,
 )
import WBPS.Core.FileScheme (FileScheme)
import WBPS.Core.FileScheme qualified as FileScheme
import WBPS.Core.Groth16.Setup (Setup (Setup, encryptionKeys))
import WBPS.Core.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Keys.ElGamal qualified as ElGamal
import WBPS.Core.Registration.Account (AccountCreated (AccountCreated, setup, userWalletPublicKey))
import WBPS.Core.Registration.FetchAccounts (loadAccount)
import WBPS.Core.Session.Commitment (Commitment (Commitment, id))
import WBPS.Core.Session.Commitment.Build (Input (Input, ekPowRho, messageBits), build)
import WBPS.Core.Session.FileScheme (deriveSessionDirectoryFrom)
import WBPS.Core.Session.Scalars as Scalars (
  Scalars (Scalars, ekPowRho, rho),
 )
import WBPS.Core.Session.Scalars.Compute qualified as Scalars
import WBPS.Core.Session.Session (
  CommitmentDemonstrated (
    CommitmentDemonstrated,
    commitment,
    preparedMessage,
    scalars
  ),
  Session (SessionCreated),
 )
import WBPS.Core.ZK.Message (
  PreparedMessage (..),
  prepareMessage,
 )

demonstrate ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  UserWalletPublicKey -> UnsignedTx -> m Session
demonstrate userWalletPublicKey unsignedTx =
  loadAccount userWalletPublicKey
    >>= \case
      Nothing -> throwError [AccountNotFound userWalletPublicKey]
      Just account@AccountCreated {setup = Setup {encryptionKeys = ElGamal.KeyPair {ek}}} -> do
        preparedMessage@PreparedMessage {messageBits} <- toWBPSFailure =<< prepareMessage unsignedTx
        scalars@Scalars {ekPowRho} <- Scalars.compute ek =<< ElGamal.generateElGamalExponent
        commitment <- build Input {ekPowRho, messageBits}
        SessionCreated account
          <$> save
            account
            CommitmentDemonstrated
              { preparedMessage
              , scalars
              , commitment
              }

save ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  AccountCreated -> CommitmentDemonstrated -> m CommitmentDemonstrated
save
  AccountCreated {userWalletPublicKey}
  event@CommitmentDemonstrated
    { preparedMessage
    , scalars
    , commitment = commitment@Commitment {id = sessionId}
    } = do
    sessionDirectory <- deriveSessionDirectoryFrom userWalletPublicKey sessionId
    ensureDir sessionDirectory

    demonstration <- asks (FileScheme.demonstration . FileScheme.session . FileScheme.account)
    writeTo (sessionDirectory </> [reldir|demonstrated|] </> FileScheme.preparedMessage demonstration) preparedMessage
    writeTo (sessionDirectory </> [reldir|demonstrated|] </> FileScheme.scalars demonstration) scalars
    writeTo (sessionDirectory </> [reldir|demonstrated|] </> FileScheme.commitment demonstration) commitment
    return event
