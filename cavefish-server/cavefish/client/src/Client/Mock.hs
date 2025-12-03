{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Module      : Client.Mock
-- Description : Mock client implementation for testing against the server.
--  This module provides a mock client that can register with the server,
--  prepare intents, verify proofs, and finalize transactions. It is designed
--  for testing purposes and simulates client-server interactions.
module Client.Mock (
  RunServer,
  Provisionned (..),
  Registered (..),
  initMockClient,
  mkDemonstrateCommitmentInputs,
  mkAskSubmissionInputs,
  mkFinaliseReq,
  verifyCommitProof,
  registerClient,
  register,
  fetchAccounts,
  fetchAccountsWithClient,
  getPending,
  getPendingWithClient,
  demonstrateCommitment,
  demonstrateCommitmentWithClient,
  demonstrateCommitmentWithClientAndVerifyWithClient,
  runCommit,
  askSubmission,
  askSubmissionWithClient,
  verifyCommitProofWithClient,
  verifySatisfies,
  as422,
  decodeHex,
) where

import Cardano.Api qualified as Api
import Control.Monad (when)
import Control.Monad.Error.Class (throwError)
import Core.Api.AppContext (AppM)
import Core.Api.Messages (
  CommitReq (CommitReq, bigR, txId),
  CommitResp (CommitResp, pi),
  PendingResp,
  clientSignatureMessage,
  commitH,
  pendingH,
 )
import Core.Cbor (
  ClientWitnessBundle (ClientWitnessBundle, cwbAuxNonce, cwbCiphertext, cwbTxId),
  deserialiseClientWitnessBundle,
 )
import Core.Intent (IntentW, satisfies, toInternalIntent)
import Core.Observers.Observer (intentStakeValidatorBytes)
import Core.PaymentProof (hashTxAbs, verifyPaymentProof)
import Core.SP.AskSubmission qualified as AskSubmission
import Core.SP.DemonstrateCommitment qualified as DemonstrateCommitment
import Core.SP.FetchAccounts qualified as FetchAccounts
import Core.SP.Register qualified as Register
import Data.Aeson (Value)
import Data.Bifunctor (first)
import Data.ByteArray qualified as BA
import Data.ByteArray.Encoding qualified as BAE
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Servant (Handler, ServerError, err422, errBody)
import WBPS.Core.Keys.Ed25519 as Ed25519 (
  KeyPair,
  UserWalletPublicKey,
  publicKey,
  userWalletPK,
 )
import WBPS.Core.Keys.ElGamal qualified as ElGamal

type RunServer = forall a. AppM a -> Handler a

data Provisionned = Provisionned
  { server :: RunServer
  , cardanoWalletKeyPair :: Ed25519.KeyPair
  }

data Registered = Registered
  { provisionned :: Provisionned
  , verificationContext :: Value
  , encryptionKey :: ElGamal.EncryptionKey
  }

initMockClient :: RunServer -> Ed25519.KeyPair -> Provisionned
initMockClient server cardanoWalletKeyPair = Provisionned {..}

-- | Create a PrepareReq from the given client ID and intent.
mkDemonstrateCommitmentInputs ::
  UserWalletPublicKey -> IntentW -> Either Text DemonstrateCommitment.Inputs
mkDemonstrateCommitmentInputs clientId intentW = do
  internalIntent <- toInternalIntent intentW
  observerBytes <- intentStakeValidatorBytes internalIntent
  pure DemonstrateCommitment.Inputs {intent = intentW, observer = Just observerBytes, clientId}

-- | Create a FinaliseReq from the given secret key, transaction ID, and transaction abstract hash.
mkAskSubmissionInputs :: Ed25519.KeyPair -> Text -> ByteString -> AskSubmission.Inputs
mkAskSubmissionInputs _ txId txAbsHash =
  let message = clientSignatureMessage txAbsHash
      signature = message -- Ed.sign secretKey (Ed.toPublic secretKey) message
   in AskSubmission.Inputs {txId = txId, lcSig = BA.convert signature}

mkFinaliseReq :: Ed25519.KeyPair -> Text -> ByteString -> AskSubmission.Inputs
mkFinaliseReq = mkAskSubmissionInputs

verifyCommitProof ::
  Ed25519.KeyPair -> DemonstrateCommitment.Outputs -> CommitResp -> Either Text ()
verifyCommitProof keypair DemonstrateCommitment.Outputs {txId = txIdText, txAbs, witnessBundleHex} CommitResp {pi = piGiven} = do
  witnessBytes <- decodeHex "witness bundle" witnessBundleHex
  ClientWitnessBundle {cwbCiphertext = ciphertext, cwbAuxNonce = auxNonceBytes, cwbTxId = bundleTxId} <-
    first (const "failed to decode witness bundle") (deserialiseClientWitnessBundle witnessBytes)
  txId <-
    case Api.deserialiseFromRawBytesHex @Api.TxId (TE.encodeUtf8 txIdText) of
      Left err -> Left (Text.pack ("failed to deserialise tx id: " <> show err))
      Right v -> Right v
  let expectedTxIdBytes = Api.serialiseToRawBytes txId
  when (bundleTxId /= expectedTxIdBytes) $ Left "witness bundle tx id mismatch"
  verifyPaymentProof (Ed25519.publicKey keypair) piGiven txAbs txId ciphertext auxNonceBytes

-- | Register the client with the server.
registerClient :: RunServer -> Register.Inputs -> Handler Register.Outputs
registerClient runServer inputs = runServer $ Register.handle inputs

-- | Register the mock client with the server.
register :: Provisionned -> Handler Registered
register provisionned@Provisionned {..} = do
  Register.Outputs {ek, publicVerificationContext} <-
    registerClient
      server
      Register.Inputs
        { userWalletPublicKey = userWalletPK cardanoWalletKeyPair
        }
  pure
    Registered
      { provisionned = provisionned
      , verificationContext = publicVerificationContext
      , encryptionKey = ek
      }

-- | List clients from the server.
fetchAccounts :: RunServer -> Handler FetchAccounts.Outputs
fetchAccounts run = run FetchAccounts.handle

-- | List clients using the given mock client.
fetchAccountsWithClient :: Registered -> Handler FetchAccounts.Outputs
fetchAccountsWithClient Registered {provisionned = Provisionned {..}} =
  fetchAccounts server

-- | List pending transactions from the server.
getPending :: RunServer -> Handler PendingResp
getPending run = run pendingH

-- | List pending transactions using the given mock client.
getPendingWithClient :: Registered -> Handler PendingResp
getPendingWithClient Registered {provisionned = Provisionned {..}} =
  getPending server

-- | Prepare an intent with the server.
demonstrateCommitment ::
  RunServer -> UserWalletPublicKey -> IntentW -> Handler DemonstrateCommitment.Outputs
demonstrateCommitment run clientId intentW =
  case mkDemonstrateCommitmentInputs clientId intentW of
    Left err -> throwError (as422 err)
    Right req -> run (DemonstrateCommitment.handle req)

-- | Prepare an intent using the given mock client.
demonstrateCommitmentWithClient :: Registered -> IntentW -> Handler DemonstrateCommitment.Outputs
demonstrateCommitmentWithClient Registered {provisionned = Provisionned {..}} intentW = do
  demonstrateCommitment server (userWalletPK cardanoWalletKeyPair) intentW

-- | Submit a commit message (big R) for the given transaction.
runCommit :: RunServer -> Text -> Ed25519.KeyPair -> Handler CommitResp
runCommit run txId keyPair = run (commitH CommitReq {txId, bigR = Ed25519.publicKey keyPair})

-- | Prepare an intent and verify the proof using the given mock client.
demonstrateCommitmentWithClientAndVerifyWithClient ::
  Registered -> IntentW -> Handler DemonstrateCommitment.Outputs
demonstrateCommitmentWithClientAndVerifyWithClient mockClient intentW = do
  demonstrateCommitmentWithClient mockClient intentW

-- | Finalise a prepared transaction with the server.
askSubmission ::
  RunServer -> Ed25519.KeyPair -> DemonstrateCommitment.Outputs -> Handler AskSubmission.Outputs
askSubmission run secretKey DemonstrateCommitment.Outputs {txId, txAbs} =
  let txAbsHash = hashTxAbs txAbs
      req = mkAskSubmissionInputs secretKey txId txAbsHash
   in run (AskSubmission.handle req)

-- | Finalise a prepared transaction using the given mock client.
askSubmissionWithClient ::
  Registered -> DemonstrateCommitment.Outputs -> Handler AskSubmission.Outputs
askSubmissionWithClient Registered {provisionned = Provisionned {..}} = askSubmission server cardanoWalletKeyPair

-- | Verify that the prepared transaction proof is valid with the given client.
verifyCommitProofWithClient ::
  Registered -> DemonstrateCommitment.Outputs -> CommitResp -> Either Text ()
verifyCommitProofWithClient Registered {provisionned = Provisionned {..}} = verifyCommitProof cardanoWalletKeyPair

-- | Verify that the prepared transaction satisfies the intent.
verifySatisfies :: IntentW -> DemonstrateCommitment.Outputs -> Either Text Bool
verifySatisfies intentW DemonstrateCommitment.Outputs {txAbs, changeDelta} = do
  internal <- toInternalIntent intentW
  pure (satisfies changeDelta internal txAbs)

as422 :: Text -> ServerError
as422 t = err422 {errBody = BL.fromStrict (TE.encodeUtf8 t)}

decodeHex :: Text -> Text -> Either Text ByteString
decodeHex label hexText =
  case BAE.convertFromBase BAE.Base16 (TE.encodeUtf8 hexText) of
    Left err -> Left (Text.concat ["failed to decode ", label, ": ", Text.pack err])
    Right bs -> Right bs
