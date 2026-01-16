{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module WBPS.Core.Session.Demonstration.Demonstrate (
  demonstrate,
) where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader.Class (asks)
import Data.Default (Default (def))
import Path (reldir, (</>))
import Path.IO (ensureDir)
import WBPS.Adapter.Path (writeTo)
import WBPS.Core.Failure (
  WBPSFailure (AccountNotFound),
 )
import WBPS.Core.Registration.Artefacts.Groth16.Setup (Setup (Setup, encryptionKeys))
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Registration.Artefacts.Keys.ElGamal qualified as ElGamal
import WBPS.Core.Registration.FetchAccounts (loadAccount)
import WBPS.Core.Registration.Registered (Registered (Registered, setup, userWalletPublicKey))
import WBPS.Core.Session.Demonstration.Artefacts.Cardano.UnsignedTx (UnsignedTx)
import WBPS.Core.Session.Demonstration.Artefacts.Commitment (Commitment (Commitment, id))
import WBPS.Core.Session.Demonstration.Artefacts.Commitment.Build (Input (Input), build)
import WBPS.Core.Session.Demonstration.Artefacts.PreparedMessage (CircuitMessage (message), circuit)
import WBPS.Core.Session.Demonstration.Artefacts.PreparedMessage.Prepare (prepare)
import WBPS.Core.Session.Demonstration.Artefacts.Rho qualified as Rho
import WBPS.Core.Session.Demonstration.Artefacts.Scalars as Scalars (
  Scalars (Scalars, ekPowRho),
 )
import WBPS.Core.Session.Demonstration.Artefacts.Scalars.Compute qualified as Scalars
import WBPS.Core.Session.Demonstration.Demonstrated (
  CommitmentDemonstrated (
    CommitmentDemonstrated,
    commitment,
    preparedMessage,
    scalars
  ),
 )
import WBPS.Core.Session.FileScheme (deriveSessionDirectoryFrom)
import WBPS.Core.Session.Session (Session (Demonstrated))
import WBPS.Core.Setup.Circuit.FileScheme (FileScheme)
import WBPS.Core.Setup.Circuit.FileScheme qualified as FileScheme

demonstrate ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  UserWalletPublicKey -> UnsignedTx -> m Session
demonstrate userWalletPublicKey unsignedTx =
  loadAccount userWalletPublicKey
    >>= \case
      Nothing -> throwError [AccountNotFound (show userWalletPublicKey)]
      Just account@Registered {setup = Setup {encryptionKeys = ElGamal.KeyPair {ek}}} -> do
        preparedMessage <- prepare def unsignedTx
        scalars@Scalars {ekPowRho} <- Scalars.compute ek =<< Rho.generateElGamalExponent
        commitment <- build userWalletPublicKey . Input ekPowRho . message . circuit $ preparedMessage
        Demonstrated account
          <$> save
            account
            CommitmentDemonstrated
              { preparedMessage
              , scalars
              , commitment
              }

save ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  Registered -> CommitmentDemonstrated -> m CommitmentDemonstrated
save
  Registered {userWalletPublicKey}
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
