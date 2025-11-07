{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

-- | Module      : Client.Mock
-- Description : Mock client implementation for testing against the server.
--  This module provides a mock client that can register with the server,
--  prepare intents, verify proofs, and finalize transactions. It is designed
--  for testing purposes and simulates client-server interactions.
module Client.Mock where

import Cardano.Api qualified as Api
import Control.Monad (when)
import Control.Monad.Error.Class (throwError)
import Core.Api.AppContext (AppM)
import Core.Api.Messages (
  ClientsResp,
  CommitReq (CommitReq, bigR, txId),
  CommitResp (CommitResp, pi),
  FinaliseReq (FinaliseReq, lcSig, txId),
  FinaliseResp,
  PendingResp,
  PrepareReq (PrepareReq, clientId, intent, observer),
  PrepareResp (PrepareResp, changeDelta, txAbs, txId, witnessBundleHex),
  RegisterReq (RegisterReq, publicKey),
  RegisterResp (RegisterResp, id, spPk),
  clientSignatureMessage,
  clientsH,
  commitH,
  finaliseH,
  pendingH,
  prepareH,
  registerH,
 )
import Core.Api.State (ClientId (ClientId))
import Core.Cbor (
  ClientWitnessBundle (ClientWitnessBundle, cwbAuxNonce, cwbCiphertext, cwbTxId),
  deserialiseClientWitnessBundle,
 )
import Core.Intent (IntentW, satisfies, toInternalIntent)
import Core.Observers.Observer (intentStakeValidatorBytes)
import Core.PaymentProof (hashTxAbs, verifyPaymentProof)
import Crypto.PubKey.Ed25519 qualified as Ed
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
  }

initMockClient :: RunServer -> Ed.SecretKey -> UnregisteredMockClient
initMockClient run lcSk =
  UnregisteredMockClient
    { umcRun = run
    , umcLcSk = lcSk
    }

-- | Create a PrepareReq from the given client ID and intent.
mkPrepareReq :: ClientId -> IntentW -> Either Text PrepareReq
mkPrepareReq clientId intentW = do
  internalIntent <- toInternalIntent intentW
  observerBytes <- intentStakeValidatorBytes internalIntent
  pure PrepareReq {intent = intentW, observer = Just observerBytes, clientId}

-- | Create a FinaliseReq from the given secret key, transaction ID, and transaction abstract hash.
mkFinaliseReq :: Ed.SecretKey -> Text -> ByteString -> FinaliseReq
mkFinaliseReq secretKey txId txAbsHash =
  let message = clientSignatureMessage txAbsHash
      signature = Ed.sign secretKey (Ed.toPublic secretKey) message
   in FinaliseReq {txId = txId, lcSig = BA.convert signature}

verifyCommitProof :: Ed.PublicKey -> PrepareResp -> CommitResp -> Either Text ()
verifyCommitProof publicKey PrepareResp {txId = txIdText, txAbs, witnessBundleHex} CommitResp {pi} = do
  witnessBytes <- decodeHex "witness bundle" witnessBundleHex
  ClientWitnessBundle {cwbCiphertext = ciphertext, cwbAuxNonce = auxNonceBytes, cwbTxId = bundleTxId} <-
    first (const "failed to decode witness bundle") (deserialiseClientWitnessBundle witnessBytes)
  txId <-
    case Api.deserialiseFromRawBytesHex @Api.TxId (TE.encodeUtf8 txIdText) of
      Left err -> Left (Text.pack ("failed to deserialise tx id: " <> show err))
      Right v -> Right v
  let expectedTxIdBytes = Api.serialiseToRawBytes txId
  when (bundleTxId /= expectedTxIdBytes) $ Left "witness bundle tx id mismatch"
  verifyPaymentProof publicKey pi txAbs txId ciphertext auxNonceBytes

-- | Register the client with the server.
registerClient :: RunServer -> Ed.PublicKey -> Handler RegisterResp
registerClient run publicKey = run $ registerH RegisterReq {publicKey}

-- | Register the mock client with the server.
register :: UnregisteredMockClient -> Handler MockClient
register UnregisteredMockClient {..} = do
  RegisterResp {id = uuid, spPk} <- registerClient umcRun (Ed.toPublic umcLcSk)
  pure MockClient {mcRun = umcRun, mcLcSk = umcLcSk, mcSpPk = spPk, mcClientId = ClientId uuid}

-- | List clients from the server.
getClients :: RunServer -> Handler ClientsResp
getClients run = run clientsH

-- | List clients using the given mock client.
getClientsWithClient :: MockClient -> Handler ClientsResp
getClientsWithClient mockClient = getClients (mcRun mockClient)

-- | List pending transactions from the server.
getPending :: RunServer -> Handler PendingResp
getPending run = run pendingH

-- | List pending transactions using the given mock client.
getPendingWithClient :: MockClient -> Handler PendingResp
getPendingWithClient mockClient = getPending (mcRun mockClient)

-- | Prepare an intent with the server.
prepare :: RunServer -> ClientId -> IntentW -> Handler PrepareResp
prepare run clientId intentW =
  case mkPrepareReq clientId intentW of
    Left err -> throwError (as422 err)
    Right req -> run (prepareH req)

-- | Prepare an intent using the given mock client.
prepareWithClient :: MockClient -> IntentW -> Handler PrepareResp
prepareWithClient mockClient intentW = do
  prepare (mcRun mockClient) mockClient.mcClientId intentW

-- | Submit a commit message (big R) for the given transaction.
runCommit :: RunServer -> Text -> Ed.PublicKey -> Handler CommitResp
runCommit run txId bigR = run (commitH CommitReq {txId, bigR})

-- | Prepare an intent and verify the proof using the given mock client.
prepareAndVerifyWithClient :: MockClient -> IntentW -> Handler PrepareResp
prepareAndVerifyWithClient mockClient intentW = do
  prepareWithClient mockClient intentW

-- | Finalise a prepared transaction with the server.
finalise :: RunServer -> Ed.SecretKey -> PrepareResp -> Handler FinaliseResp
finalise run secretKey PrepareResp {txId, txAbs} =
  let txAbsHash = hashTxAbs txAbs
      req = mkFinaliseReq secretKey txId txAbsHash
   in run (finaliseH req)

-- | Finalise a prepared transaction using the given mock client.
finaliseWithClient :: MockClient -> PrepareResp -> Handler FinaliseResp
finaliseWithClient mockClient = finalise (mcRun mockClient) (mcLcSk mockClient)

-- | Verify that the prepared transaction proof is valid with the given client.
verifyCommitProofWithClient :: MockClient -> PrepareResp -> CommitResp -> Either Text ()
verifyCommitProofWithClient mockClient = verifyCommitProof (mcSpPk mockClient)

-- | Verify that the prepared transaction satisfies the intent.
verifySatisfies :: IntentW -> PrepareResp -> Either Text Bool
verifySatisfies intentW PrepareResp {txAbs, changeDelta} = do
  internal <- toInternalIntent intentW
  pure (satisfies changeDelta internal txAbs)

as422 :: Text -> ServerError
as422 t = err422 {errBody = BL.fromStrict (TE.encodeUtf8 t)}

decodeHex :: Text -> Text -> Either Text ByteString
decodeHex label hexText =
  case BAE.convertFromBase BAE.Base16 (TE.encodeUtf8 hexText) of
    Left err -> Left (Text.concat ["failed to decode ", label, ": ", Text.pack err])
    Right bs -> Right bs
