{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Module      : Client.Mock
-- Description : Mock client implementation for testing against the server.
--  This module provides a mock client that can register with the server,
--  prepare intents, verify proofs, and finalize transactions. It is designed
--  for testing purposes and simulates client-server interactions.
module Client.Mock (
  RunServer,
  UnregisteredMockClient (..),
  MockClient (..),
  initMockClient,
  mkDemonstrateCommitmentInputs,
  mkPrepareReq,
  mkAskSubmissionInputs,
  mkFinaliseReq,
  verifyCommitProof,
  registerClient,
  register,
  getClients,
  getClientsWithClient,
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
  Accounts,
  CommitReq (CommitReq, bigR, txId),
  CommitResp (CommitResp, pi),
  PendingResp,
  clientSignatureMessage,
  clientsH,
  commitH,
  pendingH,
 )
import Core.Api.State (ClientId)
import Core.Cbor (
  ClientWitnessBundle (ClientWitnessBundle, cwbAuxNonce, cwbCiphertext, cwbTxId),
  deserialiseClientWitnessBundle,
 )
import Core.Intent (IntentW, satisfies, toInternalIntent)
import Core.Observers.Observer (intentStakeValidatorBytes)
import Core.PaymentProof (hashTxAbs, verifyPaymentProof)
import Core.SP.AskSubmission qualified as AskSubmission
import Core.SP.DemonstrateCommitment qualified as DemonstrateCommitment
import Core.SP.Register qualified as Register
import Crypto.PubKey.Ed25519 qualified as Ed
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

type RunServer = forall a. AppM a -> Handler a

data UnregisteredMockClient = UnregisteredMockClient
  { umcRun :: RunServer
  , umcLcSk :: Ed.SecretKey
  }

data MockClient = MockClient
  { mcRun :: RunServer
  , mcLcSk :: Ed.SecretKey
  , mcSpPk :: Ed.PublicKey
  , mcClientId :: ClientId
  , mcVerificationContext :: Value
  }

initMockClient :: RunServer -> Ed.SecretKey -> UnregisteredMockClient
initMockClient run lcSk =
  UnregisteredMockClient
    { umcRun = run
    , umcLcSk = lcSk
    }

-- | Create a PrepareReq from the given client ID and intent.
mkDemonstrateCommitmentInputs :: ClientId -> IntentW -> Either Text DemonstrateCommitment.Inputs
mkDemonstrateCommitmentInputs clientId intentW = do
  internalIntent <- toInternalIntent intentW
  observerBytes <- intentStakeValidatorBytes internalIntent
  pure DemonstrateCommitment.Inputs {intent = intentW, observer = Just observerBytes, clientId}

mkPrepareReq :: ClientId -> IntentW -> Either Text DemonstrateCommitment.Inputs
mkPrepareReq = mkDemonstrateCommitmentInputs

-- | Create a FinaliseReq from the given secret key, transaction ID, and transaction abstract hash.
mkAskSubmissionInputs :: Ed.SecretKey -> Text -> ByteString -> AskSubmission.Inputs
mkAskSubmissionInputs secretKey txId txAbsHash =
  let message = clientSignatureMessage txAbsHash
      signature = Ed.sign secretKey (Ed.toPublic secretKey) message
   in AskSubmission.Inputs {txId = txId, lcSig = BA.convert signature}

mkFinaliseReq :: Ed.SecretKey -> Text -> ByteString -> AskSubmission.Inputs
mkFinaliseReq = mkAskSubmissionInputs

verifyCommitProof :: Ed.PublicKey -> DemonstrateCommitment.Outputs -> CommitResp -> Either Text ()
verifyCommitProof publicKey DemonstrateCommitment.Outputs {txId = txIdText, txAbs, witnessBundleHex} CommitResp {pi = piGiven} = do
  witnessBytes <- decodeHex "witness bundle" witnessBundleHex
  ClientWitnessBundle {cwbCiphertext = ciphertext, cwbAuxNonce = auxNonceBytes, cwbTxId = bundleTxId} <-
    first (const "failed to decode witness bundle") (deserialiseClientWitnessBundle witnessBytes)
  txId <-
    case Api.deserialiseFromRawBytesHex @Api.TxId (TE.encodeUtf8 txIdText) of
      Left err -> Left (Text.pack ("failed to deserialise tx id: " <> show err))
      Right v -> Right v
  let expectedTxIdBytes = Api.serialiseToRawBytes txId
  when (bundleTxId /= expectedTxIdBytes) $ Left "witness bundle tx id mismatch"
  verifyPaymentProof publicKey piGiven txAbs txId ciphertext auxNonceBytes

-- | Register the client with the server.
registerClient :: RunServer -> Register.Inputs -> Handler Register.Outputs
registerClient runServer inputs = runServer $ Register.handle inputs

-- | Register the mock client with the server.
register :: UnregisteredMockClient -> Handler MockClient
register UnregisteredMockClient {..} = do
  registerResp@Register.Outputs {ek, publicVerificationContext} <-
    registerClient
      umcRun
      Register.Inputs
      -- userWalletPublicKey = UserWalletPublicKey . PublicKey $ Ed.toPublic umcLcSk
        {
        }
  pure
    MockClient
      { mcRun = umcRun
      , mcLcSk = umcLcSk
      -- , mcSpPk = ek
      -- , mcClientId = ClientId uuid
      -- , mcVerificationContext = registerResp.verificationContext
      }

-- | List clients from the server.
getClients :: RunServer -> Handler Accounts
getClients run = run clientsH

-- | List clients using the given mock client.
getClientsWithClient :: MockClient -> Handler Accounts
getClientsWithClient mockClient = getClients (mcRun mockClient)

-- | List pending transactions from the server.
getPending :: RunServer -> Handler PendingResp
getPending run = run pendingH

-- | List pending transactions using the given mock client.
getPendingWithClient :: MockClient -> Handler PendingResp
getPendingWithClient mockClient = getPending (mcRun mockClient)

-- | Prepare an intent with the server.
demonstrateCommitment :: RunServer -> ClientId -> IntentW -> Handler DemonstrateCommitment.Outputs
demonstrateCommitment run clientId intentW =
  case mkDemonstrateCommitmentInputs clientId intentW of
    Left err -> throwError (as422 err)
    Right req -> run (DemonstrateCommitment.handle req)

-- | Prepare an intent using the given mock client.
demonstrateCommitmentWithClient :: MockClient -> IntentW -> Handler DemonstrateCommitment.Outputs
demonstrateCommitmentWithClient mockClient intentW = do
  demonstrateCommitment (mcRun mockClient) mockClient.mcClientId intentW

-- | Submit a commit message (big R) for the given transaction.
runCommit :: RunServer -> Text -> Ed.PublicKey -> Handler CommitResp
runCommit run txId bigR = run (commitH CommitReq {txId, bigR})

-- | Prepare an intent and verify the proof using the given mock client.
demonstrateCommitmentWithClientAndVerifyWithClient ::
  MockClient -> IntentW -> Handler DemonstrateCommitment.Outputs
demonstrateCommitmentWithClientAndVerifyWithClient mockClient intentW = do
  demonstrateCommitmentWithClient mockClient intentW

-- | Finalise a prepared transaction with the server.
askSubmission ::
  RunServer -> Ed.SecretKey -> DemonstrateCommitment.Outputs -> Handler AskSubmission.Outputs
askSubmission run secretKey DemonstrateCommitment.Outputs {txId, txAbs} =
  let txAbsHash = hashTxAbs txAbs
      req = mkAskSubmissionInputs secretKey txId txAbsHash
   in run (AskSubmission.handle req)

-- | Finalise a prepared transaction using the given mock client.
askSubmissionWithClient ::
  MockClient -> DemonstrateCommitment.Outputs -> Handler AskSubmission.Outputs
askSubmissionWithClient mockClient = askSubmission (mcRun mockClient) (mcLcSk mockClient)

-- | Verify that the prepared transaction proof is valid with the given client.
verifyCommitProofWithClient ::
  MockClient -> DemonstrateCommitment.Outputs -> CommitResp -> Either Text ()
verifyCommitProofWithClient mockClient = verifyCommitProof (mcSpPk mockClient)

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
