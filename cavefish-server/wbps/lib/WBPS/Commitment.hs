{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RankNTypes #-}

module WBPS.Commitment (

) where

import Cardano.Api (ConwayEra, Tx)
import Cardano.Api qualified as Api
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader)
import Data.ByteString
import GHC.Generics
import WBPS.Core.BuildCommitment hiding (Commitment)
import WBPS.Core.FileScheme
import WBPS.Core.Keys.Ed25519
import WBPS.Core.Keys.ElGamal
import WBPS.Core.Keys.ElGamal qualified as ElGamal
import WBPS.Registration

newtype Message = Message (Tx ConwayEra)

data Commitment
  = Commitment {id :: ComId, payload :: CommitmentPayload}
  deriving (Eq, Show, Generic)

data Session
  = SessionCreated
  { userWalletPublicKey :: UserWalletPublicKey
  , message :: Message
  , commitment :: Commitment
  }

createSession ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  UserWalletPublicKey -> Tx ConwayEra -> m AccountCreated
createSession userWalletPublicKey tx =
  loadAccount userWalletPublicKey
    >>= \case
      Nothing -> throwError [AccountNotFound userWalletPublicKey]
      Just AccountCreated {encryptionKeys = ElGamal.KeyPair {ek}, ..} -> do
        rho <- ElGamal.generateElGamalExponent
        commitmentScalars <- computeCommitmentScalars ek rho

        return ()

-- The part from the paper: C ← PKE.Enc(ek, m; ρ) with ek = pkePublic, m = serialiseTx tx <> auxNonceBytes
-- (ciphertext, CommitmentSeeds {seedX, seedY}) <-
--   liftEither $ first toPkeErr (encryptWithSeeds ek message rhoBytes)
-- -- Run the BuildCommitment circuit to derive comTx. comTx is the list of out_masked_chunk values
-- -- (each packed message limb plus its Poseidon-derived mask).
-- -- Parameters cover max payload (16KB tx + 32B nonce): 131,328 bits.
-- -- We use 252-bit limbs to avoid mod-p aliasing and stay aligned with the wbps_cardano verifier circuit.
-- let commitmentParams@(BuildCommitmentParams msgSize _ _) = defCommitmentParams
--     messageBits =
--       take msgSize (payloadBits message ++ repeat 0)
-- let bcInput =
--       BuildCommitmentInput seedX seedY messageBits
-- comTx <-
--   liftIO $
--     runReaderT
--       (runBuildCommitment commitmentParams bcInput)
--       wbpsScheme
--       >>= either (ioError . userError) (pure . maskedChunks)
-- let comIdVal = computeComId (BuildCommitment.Commitment comTx)
-- return Commitment (unComId comIdVal) comTx

-- Should be 128 bits long

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
