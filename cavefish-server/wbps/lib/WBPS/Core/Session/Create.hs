{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

module WBPS.Core.Session.Create (
  create,
  Session (..),
) where

import Control.Arrow ((&&&))
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader.Class (asks)
import Data.Default (def)
import GHC.Generics (Generic)
import Path (reldir, (</>))
import Path.IO (ensureDir)
import WBPS.Adapter.Path (writeTo)
import WBPS.Core.Cardano.UnsignedTx (UnsignedTx, randomizeTx, toAbstractUnsignedTx)
import WBPS.Core.Failure (
  RegistrationFailed (AccountNotFound),
 )
import WBPS.Core.FileScheme (FileScheme)
import WBPS.Core.FileScheme qualified as FileScheme
import WBPS.Core.Groth16.Setup (Setup (Setup, encryptionKeys))
import WBPS.Core.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Keys.Ed25519 qualified as Ed25519
import WBPS.Core.Keys.ElGamal qualified as ElGamal
import WBPS.Core.Registration.Account (AccountCreated (AccountCreated, setup, userWalletPublicKey))
import WBPS.Core.Registration.FetchAccounts (loadAccount)
import WBPS.Core.Session.Commitment
import WBPS.Core.Session.Commitment.Build (Input (Input, ekPowRho, messageBits), build)
import WBPS.Core.Session.Commitment.Scalars as CommitmentScalars (
  CommitmentScalars (CommitmentScalars, ekPowRho),
 )
import WBPS.Core.Session.Commitment.Scalars.Compute qualified as CommitmentScalars
import WBPS.Core.Session.FileScheme (deriveSessionDirectoryFrom)
import WBPS.Core.Session.Session (Session (..))
import WBPS.Core.ZK.Message (Message (Message), PublicMessage (PublicMessage), messageToBits, unMessage)

data CommitmentProoved = CommitmentProoved
  { session :: Session
  , bigR :: Ed25519.PublicKey
  }
  deriving (Eq, Show, Generic)

create ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  UserWalletPublicKey -> UnsignedTx -> m Session
create userWalletPublicKey unsignedTx =
  loadAccount userWalletPublicKey
    >>= \case
      Nothing -> throwError [AccountNotFound userWalletPublicKey]
      Just account@AccountCreated {setup = Setup {encryptionKeys = ElGamal.KeyPair {ek}}} -> do
        (message, messageBits) <- (Prelude.id &&& messageToBits def) . Message <$> randomizeTx unsignedTx
        rho <- ElGamal.generateElGamalExponent
        commitmentScalars@CommitmentScalars {ekPowRho} <- CommitmentScalars.compute ek rho
        commitment <- build Input {ekPowRho, messageBits}

        saveAndReturn
          SessionCreated
            { publicMessage = PublicMessage . toAbstractUnsignedTx . unMessage $ message
            , ..
            }

saveAndReturn ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  Session -> m Session
saveAndReturn session = save session >> return session

save ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  Session -> m ()
save SessionCreated {account = AccountCreated {userWalletPublicKey}, rho, message, commitmentScalars, commitment = commitment@Commitment {id = sessionId}} = do
  sessionDirectory <- deriveSessionDirectoryFrom userWalletPublicKey sessionId
  ensureDir sessionDirectory

  FileScheme.Session
    { message = messageDir
    , rho = rhoDir
    , commitment = FileScheme.BuildCommitment {scalars = scalarsDir, commitment = commitmentDir}
    } <-
    asks (FileScheme.session . FileScheme.account)

  writeTo (sessionDirectory </> messageDir) message
  writeTo (sessionDirectory </> rhoDir) rho
  writeTo (sessionDirectory </> [reldir|commitment|] </> scalarsDir) commitmentScalars
  writeTo (sessionDirectory </> [reldir|commitment|] </> commitmentDir) commitment
