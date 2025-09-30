{-# LANGUAGE RankNTypes #-}

module Client.Mock where

import Cardano.Api qualified as Api
import Control.Monad.Error.Class (throwError)
import Core.Intent (IntentW, toInternalIntent)
import Core.Proof (verifyProof)
import Crypto.PubKey.Ed25519 qualified as Ed
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Observers.Observer (intentStakeValidatorBytes)
import Servant (Handler, ServerError, err422, errBody)
import Sp.App (AppM)
import Sp.Server (
  ClientsResp (..),
  FinaliseReq (..),
  FinaliseResp (..),
  PendingResp (..),
  PrepareReq (..),
  PrepareResp (..),
  RegisterReq (..),
  RegisterResp (..),
  clientSignatureMessage,
  clientsH,
  finaliseH,
  hashTxAbs,
  pendingH,
  prepareH,
  registerH,
 )
import Sp.State (ClientId (..))

type RunServer = forall a. AppM a -> Handler a

data MockClient = MockClient
  { mcRun :: RunServer
  , mcLcSk :: Ed.SecretKey
  , mcSpPk :: Ed.PublicKey
  , mcClientId :: Maybe ClientId
  }

initMockClient :: RunServer -> Ed.SecretKey -> Ed.PublicKey -> MockClient
initMockClient run lcSk spPk =
  MockClient
    { mcRun = run
    , mcLcSk = lcSk
    , mcSpPk = spPk
    , mcClientId = Nothing
    }

mkPrepareReq :: ClientId -> IntentW -> Either Text PrepareReq
mkPrepareReq clientId intentW = do
  internalIntent <- toInternalIntent intentW
  observerBytes <- intentStakeValidatorBytes internalIntent
  pure PrepareReq{intent = intentW, observer = observerBytes, clientId}

mkFinaliseReq :: Ed.SecretKey -> Text -> ByteString -> FinaliseReq
mkFinaliseReq secretKey txId txAbsHash =
  let message = clientSignatureMessage txAbsHash
      signature = Ed.sign secretKey (Ed.toPublic secretKey) message
   in FinaliseReq{txId = txId, lcSig = BA.convert signature}

verifyPrepareProof :: Ed.PublicKey -> PrepareResp -> Either Text Bool
verifyPrepareProof publicKey PrepareResp{txId = txIdText, txAbs, proof} = do
  txId <-
    case Api.deserialiseFromRawBytesHex @Api.TxId (TE.encodeUtf8 txIdText) of
      Left err -> Left (Text.pack ("failed to deserialise tx id: " <> show err))
      Right v -> Right v
  let txAbsHash = hashTxAbs txAbs
  pure (verifyProof publicKey txId txAbsHash proof)

registerClient :: RunServer -> Ed.PublicKey -> Handler RegisterResp
registerClient run publicKey = run $ registerH RegisterReq{publicKey}

register :: MockClient -> Handler MockClient
register mockClient = do
  RegisterResp{id = uuid} <- registerClient (mcRun mockClient) (Ed.toPublic (mcLcSk mockClient))
  pure mockClient{mcClientId = Just (ClientId uuid)}

getClients :: RunServer -> Handler ClientsResp
getClients run = run clientsH

getClientsWithClient :: MockClient -> Handler ClientsResp
getClientsWithClient mockClient = getClients (mcRun mockClient)

getPending :: RunServer -> Handler PendingResp
getPending run = run pendingH

getPendingWithClient :: MockClient -> Handler PendingResp
getPendingWithClient mockClient = getPending (mcRun mockClient)

prepare :: RunServer -> ClientId -> IntentW -> Handler PrepareResp
prepare run clientId intentW =
  case mkPrepareReq clientId intentW of
    Left err -> throwError (as422 err)
    Right req -> run (prepareH req)

prepareWithClient :: MockClient -> IntentW -> Handler PrepareResp
prepareWithClient mockClient intentW = do
  clientId <- requireClientId mockClient
  prepare (mcRun mockClient) clientId intentW

prepareAndVerifyWithClient :: MockClient -> IntentW -> Handler PrepareResp
prepareAndVerifyWithClient mockClient intentW = do
  resp <- prepareWithClient mockClient intentW
  case verifyPrepareProofWithClient mockClient resp of
    Left err -> throwError (as422 err)
    Right False -> throwError (as422 "prepare proof verification failed")
    Right True -> pure resp

finalise :: RunServer -> Ed.SecretKey -> PrepareResp -> Handler FinaliseResp
finalise run secretKey PrepareResp{txId, txAbs} =
  let txAbsHash = hashTxAbs txAbs
      req = mkFinaliseReq secretKey txId txAbsHash
   in run (finaliseH req)

finaliseWithClient :: MockClient -> PrepareResp -> Handler FinaliseResp
finaliseWithClient mockClient = finalise (mcRun mockClient) (mcLcSk mockClient)

verifyPrepareProofWithClient :: MockClient -> PrepareResp -> Either Text Bool
verifyPrepareProofWithClient mockClient = verifyPrepareProof (mcSpPk mockClient)

requireClientId :: MockClient -> Handler ClientId
requireClientId MockClient{mcClientId = Just clientId} = pure clientId
requireClientId _ = throwError (as422 "mock client not registered")

as422 :: Text -> ServerError
as422 t = err422{errBody = BL.fromStrict (TE.encodeUtf8 t)}
