{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec () where

import Cardano.Api qualified as Api
import Client.Impl qualified as Client
import Client.Mock (MockClient (mcRun, mcVerificationContext), mkFinaliseReq)
import Client.Mock qualified as Mock
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO)
import Control.Monad.Trans.Except (runExceptT)
import Cooked.MockChain.MockChainState (MockChainState)
import Core.Api.AppContext (Env (spSk), runApp)
import Core.Api.Config (Config)
import Core.Api.Messages (
  ClientInfo (ClientInfo, clientId, userPublicKey),
  ClientsResp (ClientsResp, clients),
  CommitReq (CommitReq, bigR, txId),
  CommitResp (pi),
  PendingItem (PendingItem, clientId, expiresAt, txAbsHash, txId),
  PendingResp (PendingResp, pending),
  PendingSummary (PendingSummary, pendingClientId, pendingExpiresAt),
  SubmittedSummary (SubmittedSummary, submittedAt, submittedClientId, submittedTx),
  TransactionResp (TransactionMissing, TransactionPending, TransactionSubmitted),
  transactionH,
 )
import Core.Api.State (
  ClientId (ClientId),
  ClientRegistrationStore,
  CompleteStore,
  Pending (Pending, auxNonce, ciphertext, creator, expiry, rho, tx, txAbsHash),
  PendingStore,
 )
import Core.Cbor (
  ClientWitnessBundle (cwbAuxNonce, cwbCiphertext),
  deserialiseClientWitnessBundle,
 )
import Core.CborSpec qualified as CborSpec
import Core.Intent (
  BuildTxResult (BuildTxResult, changeDelta, tx, txAbs),
  IntentW,
  satisfies,
  toInternalIntent,
 )
import Core.PaymentProof (ProofResult (ProofEd25519), hashTxAbs)
import Core.Pke (ciphertextDigest)
import Core.Proof (mkProof, renderHex)
import Core.SP.AskSubmission (Inputs (..))
import Core.SP.AskSubmission qualified as AskSubmission
import Core.SP.DemonstrateCommitment (Inputs (..))
import Core.SP.DemonstrateCommitment qualified as DemonstrateCommitment
import Core.SP.Register qualified as Register
import Core.TxAbs (cardanoTxToTxAbs)
import Crypto.PubKey.Ed25519 qualified as Ed
import Data.Aeson qualified as Aeson
import Data.Bits (xor)
import Data.ByteArray qualified as BA
import Data.ByteArray.Encoding qualified as BAE
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.Default (def)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE
import Ledger.Tx (
  pattern CardanoEmulatorEraTx,
 )
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp qualified as Warp
import Path ((</>))
import Path qualified
import Servant (Handler, Proxy (Proxy), runHandler', (:<|>) ((:<|>)))
import Servant.Client (BaseUrl (BaseUrl))
import Servant.Client qualified as SC
import Servant.Server.Internal.ServerError (ServerError (ServerError, errBody, errHTTPCode))
import Sp.Emulator (buildWithCooked, initialMockState, mkCookedEnv)
import Sp.Server (CavefishApi, mkApp)
import System.Directory (createDirectoryIfMissing)
import System.FilePath qualified as FP
import System.IO.Temp (withSystemTempDirectory)
import Test.Common (
  testClientSecretKey,
  testCommitSecretKey,
  testIntentW,
  testPkeSecretKey,
  testSecretKey,
  testSpWallet,
 )
import Test.Hspec (Spec, describe, expectationFailure, it, runIO, shouldBe)
import WBPS.Core (SignerKey, WbpsPublicKey (WbpsPublicKey, wpkX, wpkY))
import WBPS.Core.FileScheme (
  FileScheme (FileScheme, accounts, verificationContext),
  mkFileSchemeFromRoot,
 )


mkEnv ::
  FileScheme ->
  PendingStore ->
  CompleteStore ->
  ClientRegistrationStore ->
  TVar MockChainState ->
  Env
mkEnv wbpsScheme pendingStore completeStore clientRegVar mockStateVar =
  let config :: Config = def
   in mkCookedEnv
        mockStateVar
        pendingStore
        completeStore
        clientRegVar
        testSecretKey
        testPkeSecretKey
        testSpWallet
        wbpsScheme
        config


spec :: Spec
spec = do
  wbpsScheme <- runIO (mkFileSchemeFromRoot "../../wbps")
  CborSpec.spec
  describe "buildTx integration" $ do
    it "demonstrateCommitment -> finalise roundtrip uses buildTx" $ do
      pendingStore <- newTVarIO Map.empty
      completeStore <- newTVarIO Map.empty
      clientRegVar <- newTVarIO Map.empty
      mockStateVar <- newTVarIO initialMockState
      let env = mkEnv wbpsScheme pendingStore completeStore clientRegVar mockStateVar
      let mockClient0 = Mock.initMockClient (runApp env) testClientSecretKey
      mockClient <- runHandlerOrFail (Mock.register mockClient0)
      case Mock.mcVerificationContext mockClient of
        Aeson.Object _ -> pure ()
        _ -> expectationFailure "verification context is not a JSON object"
      let registeredClientId = Mock.mcClientId mockClient
      prepareReq <- mkPrepareReqOrFail registeredClientId testIntentW
      internalIntent <-
        case toInternalIntent (intent prepareReq) of
          Left err -> expectationFailure (show err) >> fail "invalid buildTx intent"
          Right intent -> pure intent


    it "rejects finalise with invalid signature" $ do
      pendingStore <- newTVarIO Map.empty
      completeStore <- newTVarIO Map.empty
      clientRegVar <- newTVarIO Map.empty
      mockStateVar <- newTVarIO initialMockState
      let env = mkEnv wbpsScheme pendingStore completeStore clientRegVar mockStateVar
      let mockClient0 = Mock.initMockClient (runApp env) testClientSecretKey
      mockClient <- runHandlerOrFail (Mock.register mockClient0)
      let registeredClientId = Mock.mcClientId mockClient
      prepareReq <- mkPrepareReqOrFail registeredClientId testIntentW
      internalIntent <-
        case toInternalIntent (intent prepareReq) of
          Left err -> expectationFailure (show err) >> fail "invalid buildTx intent"
          Right intent -> pure intent



  describe "registration" $ do
    it "returns the verification context stored on disk" $ do
      expectedValue <- expectedVerificationContext wbpsScheme
      pendingStore <- newTVarIO Map.empty
      completeStore <- newTVarIO Map.empty
      clientRegVar <- newTVarIO Map.empty
      mockStateVar <- newTVarIO initialMockState
      let env = mkEnv wbpsScheme pendingStore completeStore clientRegVar mockStateVar
          mockClient0 = Mock.initMockClient (runApp env) testClientSecretKey
      mockClient <- runHandlerOrFail (Mock.register mockClient0)
      mcVerificationContext mockClient `shouldBe` expectedValue

    it "rejects registrations if the verification context JSON is invalid" $ do
      signerKey <- expectSignerKey
      let invalidPayload = "{ not json }"
      withSystemTempDirectory "cavefish-wbps-invalid" $ \tmpRoot -> do
        setupMinimalWbpsTree tmpRoot
        accountFolder <- accountDirectoryPath tmpRoot signerKey
        createDirectoryIfMissing True accountFolder
        let verificationFile = accountFolder FP.</> "verification_context.json"
        writeFile verificationFile invalidPayload
        wbpsSchemeTmp <- mkFileSchemeFromRoot tmpRoot
        pendingStore <- newTVarIO Map.empty
        completeStore <- newTVarIO Map.empty
        clientRegVar <- newTVarIO Map.empty
        mockStateVar <- newTVarIO initialMockState
        let env = mkEnv wbpsSchemeTmp pendingStore completeStore clientRegVar mockStateVar
            mockClient0 = Mock.initMockClient (runApp env) testClientSecretKey
        result <- runExceptT (runHandler' (Mock.register mockClient0))
        case result of
          Left ServerError {errHTTPCode, errBody} -> do
            errHTTPCode `shouldBe` 500
            let bodyText = BL.toStrict errBody
            (BS.isInfixOf (BS8.pack "verification context unavailable") bodyText)
              `shouldBe` True
          Right _ -> expectationFailure "registration unexpectedly succeeded"

  describe "API surfaces" $ do
    it "clients endpoint returns registered clients" $ do
      pendingStore <- newTVarIO Map.empty
      completeStore <- newTVarIO Map.empty
      clientRegVar <- newTVarIO Map.empty
      mockStateVar <- newTVarIO initialMockState
      let env = mkEnv wbpsScheme pendingStore completeStore clientRegVar mockStateVar
      let mockClient0 = Mock.initMockClient (runApp env) testClientSecretKey
      mockClient <- runHandlerOrFail (Mock.register mockClient0)
      let ClientId clientUuid = Mock.mcClientId mockClient
      ClientsResp {clients = clientInfos} <-
        runHandlerOrFail (Mock.getClientsWithClient mockClient)
      clientInfos
        `shouldBe` [ClientInfo {clientId = clientUuid, userPublicKey = expectedUserPublicKeyHex}]

    it "pending endpoint returns stored pending transactions" $ do
      pendingStore <- newTVarIO Map.empty
      completeStore <- newTVarIO Map.empty
      clientRegVar <- newTVarIO Map.empty
      mockStateVar <- newTVarIO initialMockState
      let env = mkEnv wbpsScheme pendingStore completeStore clientRegVar mockStateVar
      let mockClient0 = Mock.initMockClient (runApp env) testClientSecretKey
      mockClient <- runHandlerOrFail (Mock.register mockClient0)
      let ClientId clientUuid = Mock.mcClientId mockClient
      let registeredClientId = ClientId clientUuid
      prepareReq <- mkPrepareReqOrFail registeredClientId testIntentW
      internalIntent <-
        case toInternalIntent (intent prepareReq) of
          Left err -> expectationFailure (show err) >> fail "invalid buildTx intent"
          Right intent -> pure intent
      BuildTxResult
        { tx = expectedTx
        , txAbs = expectedTxAbs
        , changeDelta = _
        } <-
        buildWithCooked mockStateVar env internalIntent prepareReq.observer
      let expectedTxIdValue = Api.getTxId (Api.getTxBody expectedTx)
          expectedTxId = Api.serialiseToRawBytesHexText expectedTxIdValue
          expectedTxAbsHash = hashTxAbs expectedTxAbs
      _ <- runHandlerOrFail (Mock.demonstrateCommitmentWithClient mockClient testIntentW)
      pendingMap <- readTVarIO pendingStore
      case Map.lookup expectedTxIdValue pendingMap of
        Nothing -> expectationFailure "pending entry not stored"
        Just Pending {expiry = expectedExpiry, creator} -> do
          creator `shouldBe` registeredClientId
          let expectedItem =
                PendingItem
                  { txId = expectedTxId
                  , txAbsHash = renderHex expectedTxAbsHash
                  , expiresAt = expectedExpiry
                  , clientId = clientUuid
                  }
          PendingResp {pending = items} <-
            runHandlerOrFail (Mock.getPendingWithClient mockClient)
          items `shouldBe` [expectedItem]

  describe "Client implementation" $ do
    it "runIntent performs demonstrateCommitment/verify/finalise" $ do
      pendingStore <- newTVarIO Map.empty
      completeStore <- newTVarIO Map.empty
      clientRegVar <- newTVarIO Map.empty
      mockStateVar <- newTVarIO initialMockState
      let env = mkEnv wbpsScheme pendingStore completeStore clientRegVar mockStateVar
          clientEnv = mkClientEnv env
      AskSubmission.Outputs {result = finalResult} <-
        runHandlerOrFail $
          Client.withSession clientEnv $
            \session -> Client.runIntent session testIntentW
      finalResult `shouldBe` AskSubmission.Finalised
      PendingResp {pending = pendingAfter} <-
        runHandlerOrFail (Client.runClient clientEnv Client.listPending)
      pendingAfter `shouldBe` []
      ClientsResp {clients = clientInfos} <-
        runHandlerOrFail (Client.runClient clientEnv Client.listClients)
      case clientInfos of
        [ClientInfo {userPublicKey}] -> userPublicKey `shouldBe` expectedUserPublicKeyHex
        _ -> expectationFailure "expected exactly one registered client"

  describe "Http server roundtrip" $ do
    it "demonstrateCommitment -> finalise roundtrip uses buildTx" $ do
      pendingStore <- newTVarIO Map.empty
      completeStore <- newTVarIO Map.empty
      clientRegVar <- newTVarIO Map.empty
      mockStateVar <- newTVarIO initialMockState
      let env = mkEnv wbpsScheme pendingStore completeStore clientRegVar mockStateVar
      let application = pure (mkApp env)
      Warp.testWithApplication application $ \port -> do
        manager <- newManager defaultManagerSettings
        let baseUrl = BaseUrl SC.Http "127.0.0.1" port ""
            servantEnv = SC.mkClientEnv manager baseUrl
            ( registerClient :<|> demonstrateCommitmentClient :<|> askSubmissionClient :<|> commitClient
                :<|> clientsClient
                :<|> pendingClient
                :<|> transactionClient
              ) =
                SC.client (Proxy @CavefishApi)
