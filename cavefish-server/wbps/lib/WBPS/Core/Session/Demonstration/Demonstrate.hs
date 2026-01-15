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
import WBPS.Core.Cardano.UnsignedTx (UnsignedTx)
import WBPS.Core.Failure (
  WBPSFailure (AccountNotFound),
 )
import WBPS.Core.FileScheme (FileScheme)
import WBPS.Core.FileScheme qualified as FileScheme
import WBPS.Core.Groth16.Setup (Setup (Setup, encryptionKeys))
import WBPS.Core.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Keys.ElGamal qualified as ElGamal
import WBPS.Core.Registration.FetchAccounts (loadAccount)
import WBPS.Core.Registration.Registered (Registered (Registered, setup, userWalletPublicKey))
import WBPS.Core.Session.Demonstration.Commitment (Commitment (Commitment, id))
import WBPS.Core.Session.Demonstration.Commitment.Build (Input (Input), build)
import WBPS.Core.Session.Demonstration.Demonstrated (
  CommitmentDemonstrated (
    CommitmentDemonstrated,
    commitment,
    preparedMessage,
    scalars
  ),
 )
import WBPS.Core.Session.Demonstration.PreparedMessage (CircuitMessage (message), circuit)
import WBPS.Core.Session.Demonstration.PreparedMessage.Prepare (prepare)
import WBPS.Core.Session.Demonstration.Scalars as Scalars (
  Scalars (Scalars, ekPowRho),
 )
import WBPS.Core.Session.Demonstration.Scalars.Compute qualified as Scalars
import WBPS.Core.Session.FileScheme (deriveSessionDirectoryFrom)
import WBPS.Core.Session.Session (Session (Demonstrated))

demonstrate ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  UserWalletPublicKey -> UnsignedTx -> m Session
demonstrate userWalletPublicKey unsignedTx =
  loadAccount userWalletPublicKey
    >>= \case
      Nothing -> throwError [AccountNotFound (show userWalletPublicKey)]
      Just account@Registered {setup = Setup {encryptionKeys = ElGamal.KeyPair {ek}}} -> do
        preparedMessage <- prepare def unsignedTx
        scalars@Scalars {ekPowRho} <- Scalars.compute ek =<< ElGamal.generateElGamalExponent
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
