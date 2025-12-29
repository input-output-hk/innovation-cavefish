{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RankNTypes #-}

module WBPS.Core.Session.Create (
  create,
  Session (..),
) where

import Control.Arrow ((&&&))
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (def)
import GHC.Generics (Generic)
import WBPS.Core.Cardano.UnsignedTx (UnsignedTx, randomizeTx, toAbstractUnsignedTx)
import WBPS.Core.Failure (
  RegistrationFailed (AccountNotFound),
 )
import WBPS.Core.FileScheme (FileScheme)
import WBPS.Core.Groth16.Setup (Setup (Setup, encryptionKeys))
import WBPS.Core.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Keys.ElGamal (
  Rho,
 )
import WBPS.Core.Keys.ElGamal qualified as ElGamal
import WBPS.Core.Registration.Account (AccountCreated (AccountCreated, setup))
import WBPS.Core.Registration.FetchAccounts (loadAccount)
import WBPS.Core.Session.Commitment.Build (Commitment, Input (Input, ekPowRho, messageBits), build)
import WBPS.Core.Session.Commitment.Scalars as CommitmentScalars (
  CommitmentScalars (CommitmentScalars, ekPowRho),
  compute,
 )
import WBPS.Core.ZK.Message (Message (Message), MessageBits, PublicMessage (PublicMessage), messageToBits)

data Session
  = SessionCreated
  { userWalletPublicKey :: UserWalletPublicKey
  , setup :: Setup
  , message :: Message
  , messageBits :: MessageBits
  , publicMessage :: PublicMessage
  , rho :: Rho
  , commitmentScalars :: CommitmentScalars
  , commitment :: Commitment
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

create ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  UserWalletPublicKey -> UnsignedTx -> m Session
create userWalletPublicKey unsignedTx =
  loadAccount userWalletPublicKey
    >>= \case
      Nothing -> throwError [AccountNotFound userWalletPublicKey]
      Just AccountCreated {setup = setup@Setup {encryptionKeys = ElGamal.KeyPair {ek}}} -> do
        (message, messageBits) <- (id &&& messageToBits def) . Message <$> randomizeTx unsignedTx
        rho <- ElGamal.generateElGamalExponent
        commitmentScalars@CommitmentScalars {ekPowRho} <- CommitmentScalars.compute ek rho
        commitment <- build Input {ekPowRho, messageBits}
        return
          SessionCreated
            { publicMessage = PublicMessage . toAbstractUnsignedTx $ unsignedTx
            , ..
            }
