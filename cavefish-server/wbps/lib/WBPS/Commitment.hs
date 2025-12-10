{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RankNTypes #-}

module WBPS.Commitment (
  createSession,
  Session (..),
  Message (..),
  PublicMessage (..),
  Commitment (..),
) where

import Cardano.Api (ConwayEra, Tx)
import Cardano.Api qualified as Api
import Cardano.Api.Shelley (Tx (..))
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..))
import Cardano.Ledger.Api qualified as Ledger
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Identity
import Control.Monad.Reader (MonadReader)
import Data.Aeson
import Data.Bits (testBit)
import Data.ByteString hiding (concatMap, take)
import Data.ByteString qualified as BS
import GHC.Generics
import Ledger (CardanoTx (CardanoEmulatorEraTx))
import WBPS.Core.BuildCommitment
import WBPS.Core.Cardano.Cbor (serialiseTx)
import WBPS.Core.FileScheme
import WBPS.Core.Keys.Ed25519
import WBPS.Core.Keys.ElGamal
import WBPS.Core.Keys.ElGamal qualified as ElGamal
import WBPS.Core.Primitives.Circom (BuildCommitmentParams (..), defCommitmentParams)
import WBPS.Registration

newtype Message = Message (Tx ConwayEra)
  deriving newtype (Eq, Show, FromJSON, ToJSON)

newtype PublicMessage = PublicMessage (Tx ConwayEra)
  deriving newtype (Eq, Show, FromJSON, ToJSON)

data Session
  = SessionCreated
  { userWalletPublicKey :: UserWalletPublicKey
  , message :: Message
  , rho :: Rho
  , commitmentScalars :: CommitmentScalars
  , commitment :: Commitment
  , publicMessage :: PublicMessage
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

createSession ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  UserWalletPublicKey -> Tx ConwayEra -> m Session
createSession userWalletPublicKey tx =
  loadAccount userWalletPublicKey
    >>= \case
      Nothing -> throwError [AccountNotFound userWalletPublicKey]
      Just AccountCreated {encryptionKeys = ElGamal.KeyPair {ek}, ..} -> do
        rho <- ElGamal.generateElGamalExponent
        commitmentScalars@CommitmentScalars {ekPowRho} <- computeCommitmentScalars ek rho
        message <- randomizeTx tx
        commitment <- builCommitment ekPowRho message
        return
          SessionCreated
            { publicMessage = PublicMessage . txToTxAbs $ tx
            , ..
            }

txToTxAbs :: Api.Tx Api.ConwayEra -> Tx Api.ConwayEra
txToTxAbs (ShelleyTx era tx@AlonzoTx {body}) =
  let strippedBody = setInputs mempty body
   in ShelleyTx era tx {body = strippedBody}
  where
    setInputs ins = runIdentity . Ledger.inputsTxBodyL (\_ -> Identity ins)

-- should add aux in metadata
randomizeTx :: MonadIO m => Tx ConwayEra -> m Message
randomizeTx = pure . Message

builCommitment ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  AffinePoint ->
  Message ->
  m Commitment
builCommitment ekPowRho (Message message) = do
  let commitmentParams@(BuildCommitmentParams msgSize _ _) = defCommitmentParams
      messageBits = take msgSize (payloadBits (serialiseTx message) ++ repeat 0)

  BuildCommitmentOutput {maskedChunks} <-
    toWBPSFailure =<< runBuildCommitment commitmentParams BuildCommitmentInput {ekPowRho, messageBits}
  return $ mkCommitment (CommitmentPayload maskedChunks)

computeCommitmentScalars ::
  MonadError [RegistrationFailed] m => EncryptionKey -> Rho -> m CommitmentScalars
computeCommitmentScalars ek rho =
  CommitmentScalars
    <$> toWBPSFailure (encryptionKeyPowRho ek rho)
    <*> toWBPSFailure (generatorPowRho rho)

toWBPSFailure :: MonadError [RegistrationFailed] m => Either String a -> m a
toWBPSFailure = either (throwError . pure . BuildCommitmentFailed) pure

data CommitmentScalars
  = CommitmentScalars
  { ekPowRho :: AffinePoint
  , gPowRho :: AffinePoint
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- Convert a bytestring to a little-endian bit vector.
payloadBits :: ByteString -> [Int]
payloadBits bs = concatMap byteToBits (BS.unpack bs)
  where
    byteToBits b =
      [ if testBit b i then 1 else 0
      | i <- [0 .. 7]
      ]
