{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec (spec) where

import Blammo.Logging.Simple (Logger, defaultLogSettings, newLogger)
import Cardano.Api qualified as Api
import Client.Impl qualified as Client
import Client.Mock (MockClient (mcRun, mcVerificationContext), mkFinaliseReq)
import Client.Mock qualified as Mock
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO)
import Control.Monad.Trans.Except (runExceptT)
import Cooked.MockChain.MockChainState (MockChainState)
import Core.Api.AppContext (Env (spSk), runApp)
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

runHandlerOrFail :: Handler a -> IO a
runHandlerOrFail handler = do
  result <- runExceptT (runHandler' handler)
  either (fail . show) pure result

runClientOrFail :: SC.ClientEnv -> SC.ClientM a -> IO a
runClientOrFail clientEnv action = do
  result <- SC.runClientM action clientEnv
  case result of
    Left err -> expectationFailure ("HTTP client call failed: " <> show err) >> fail "http client failure"
    Right value -> pure value

mkEnv ::
  FileScheme ->
  PendingStore ->
  CompleteStore ->
  ClientRegistrationStore ->
  TVar MockChainState ->
  Logger ->
  Env
mkEnv wbpsScheme pendingStore completeStore clientRegVar mockStateVar logger =
  mkCookedEnv
    mockStateVar
    pendingStore
    completeStore
    clientRegVar
    testSecretKey
    testPkeSecretKey
    testSpWallet
    wbpsScheme
    logger

expectedUserPublicKeyHex :: Text
expectedUserPublicKeyHex =
  renderHex (BA.convert (Ed.toPublic testClientSecretKey))

testWbpsPublicKey :: WbpsPublicKey
testWbpsPublicKey =
  WbpsPublicKey
    { wpkX = BS.replicate 32 0x11
    , wpkY = BS.replicate 32 0x22
    }

mkClientEnv :: Env -> Client.ClientEnv
mkClientEnv env =
  Client.ClientEnv
    { Client.run = runApp env
    , Client.lcSk = testClientSecretKey
    }

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
      logger <- newLogger defaultLogSettings
      let env = mkEnv wbpsScheme pendingStore completeStore clientRegVar mockStateVar logger
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

      -- TODO WG: We can't do this exactly, but it'd be nice to check coherance of the observer with the intent
      -- expectedObserverBytes <-
      --   case intentStakeValidatorBytes internalIntent of
      --     Left err -> expectationFailure (show err) >> fail "invalid observer"
      --     Right bytes -> pure bytes
      -- expectedObserverBytes `shouldBe` prepareReq.observer
      BuildTxResult
        { tx = expectedTx
        , txAbs = expectedTxAbs
        , changeDelta = expectedDelta
        } <-
        buildWithCooked mockStateVar env internalIntent prepareReq.observer
      let expectedTxIdValue = Api.getTxId (Api.getTxBody expectedTx)
          expectedTxId = Api.serialiseToRawBytesHexText expectedTxIdValue
          expectedTxAbsHash = hashTxAbs expectedTxAbs
          fetchTransaction txIdTxt =
            runExceptT (runHandler' (runApp env (transactionH txIdTxt)))
          ClientId expectedClientUuid = registeredClientId

      beforeExists <- fetchTransaction expectedTxId
      case beforeExists of
        Right TransactionMissing -> pure ()
        Right resp -> expectationFailure ("expected missing transaction but got " <> show resp)
        Left err -> expectationFailure ("expected missing transaction but got error: " <> show err)

      prepareResp@DemonstrateCommitment.Outputs {txId = gotTxId, txAbs = gotTxAbs, witnessBundleHex} <-
        runHandlerOrFail (Mock.demonstrateCommitmentWithClient mockClient testIntentW)
      gotTxId `shouldBe` expectedTxId
      gotTxAbs `shouldBe` expectedTxAbs
      satisfies expectedDelta internalIntent gotTxAbs `shouldBe` True
      pendingMap <- readTVarIO pendingStore
      pendingEntry <-
        case Map.lookup expectedTxIdValue pendingMap of
          Nothing -> expectationFailure "pending entry not stored" >> fail "missing pending entry"
          Just entry -> pure entry
      let Pending
            { tx = storedTx
            , txAbsHash = storedTxAbsHash
            , expiry = storedExpiry
            , creator = storedCreator
            , ciphertext = storedCiphertext
            , auxNonce = storedAuxNonce
            , rho = _storedRho
            } = pendingEntry
          storedCipherDigest = ciphertextDigest storedCiphertext
          expectedProof = ProofEd25519 (mkProof (spSk env) expectedTxIdValue expectedTxAbsHash storedCipherDigest)
      witnessBundleBytes <-
        case BAE.convertFromBase BAE.Base16 (TE.encodeUtf8 witnessBundleHex) of
          Left err ->
            expectationFailure ("failed to decode witness bundle hex: " <> err) >> fail "invalid witness bundle"
          Right bs -> pure bs
      clientBundle <-
        case deserialiseClientWitnessBundle witnessBundleBytes of
          Left err ->
            expectationFailure ("failed to decode witness bundle: " <> Text.unpack err)
              >> fail "invalid witness bundle"
          Right bundle -> pure bundle
      cwbCiphertext clientBundle `shouldBe` storedCiphertext
      cwbAuxNonce clientBundle `shouldBe` storedAuxNonce
      storedTx `shouldBe` expectedTx
      storedTxAbsHash `shouldBe` expectedTxAbsHash
      let storedTxAbs = cardanoTxToTxAbs (CardanoEmulatorEraTx storedTx)
      satisfies expectedDelta internalIntent storedTxAbs `shouldBe` True
      storedCreator `shouldBe` registeredClientId

      pendingStatus <- fetchTransaction expectedTxId
      case pendingStatus of
        Right (TransactionPending PendingSummary {pendingExpiresAt, pendingClientId}) -> do
          pendingExpiresAt `shouldBe` storedExpiry
          pendingClientId `shouldBe` expectedClientUuid
        Right resp -> expectationFailure ("expected pending transaction but got " <> show resp)
        Left err -> expectationFailure ("expected pending transaction but got error: " <> show err)

      let commitBigR = Ed.toPublic testCommitSecretKey
      commitResp <- runHandlerOrFail (Mock.runCommit (mcRun mockClient) gotTxId commitBigR)
      Mock.verifyCommitProofWithClient mockClient prepareResp commitResp `shouldBe` Right ()
      commitResp.pi `shouldBe` expectedProof

      AskSubmission.Outputs {txId = finalTxId, result = finalResult, submittedAt = finalSubmittedAt} <-
        runHandlerOrFail (Mock.finaliseWithClient mockClient prepareResp)
      pendingAfter <- readTVarIO pendingStore
      Map.notMember expectedTxIdValue pendingAfter `shouldBe` True
      finalTxId `shouldBe` gotTxId
      finalResult `shouldBe` AskSubmission.Finalised

      submittedStatus <- fetchTransaction expectedTxId
      case submittedStatus of
        Right (TransactionSubmitted SubmittedSummary {submittedTx, submittedAt, submittedClientId}) -> do
          submittedTx `shouldBe` CardanoEmulatorEraTx expectedTx
          submittedAt `shouldBe` finalSubmittedAt
          submittedClientId `shouldBe` expectedClientUuid
        Right resp -> expectationFailure ("expected submitted transaction but got " <> show resp)
        Left err ->
          expectationFailure ("expected submitted transaction but got error: " <> show err)

    it "rejects finalise with invalid signature" $ do
      pendingStore <- newTVarIO Map.empty
      completeStore <- newTVarIO Map.empty
      clientRegVar <- newTVarIO Map.empty
      mockStateVar <- newTVarIO initialMockState
      logger <- newLogger defaultLogSettings
      let env = mkEnv wbpsScheme pendingStore completeStore clientRegVar mockStateVar logger
      let mockClient0 = Mock.initMockClient (runApp env) testClientSecretKey
      mockClient <- runHandlerOrFail (Mock.register mockClient0)
      let registeredClientId = Mock.mcClientId mockClient
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
      Map.member expectedTxIdValue pendingMap `shouldBe` True
      let commitBigR = Ed.toPublic testCommitSecretKey
      _ <- runHandlerOrFail (Mock.runCommit (mcRun mockClient) expectedTxId commitBigR)

      let validFinaliseReq@AskSubmission.Inputs {} =
            Mock.mkFinaliseReq (Mock.mcLcSk mockClient) expectedTxId expectedTxAbsHash
          invalidFinaliseReq =
            validFinaliseReq
              { lcSig = corruptSignature (lcSig validFinaliseReq)
              }
      AskSubmission.Outputs {txId = finalTxId, result = finalResult} <-
        runHandlerOrFail (runApp env $ AskSubmission.handle invalidFinaliseReq)
      finalTxId `shouldBe` expectedTxId
      finalResult `shouldBe` AskSubmission.Rejected "invalid client signature"
      pendingAfter <- readTVarIO pendingStore
      Map.member expectedTxIdValue pendingAfter `shouldBe` True

  describe "registration" $ do
    it "returns the verification context stored on disk" $ do
      expectedValue <- expectedVerificationContext wbpsScheme
      pendingStore <- newTVarIO Map.empty
      completeStore <- newTVarIO Map.empty
      clientRegVar <- newTVarIO Map.empty
      mockStateVar <- newTVarIO initialMockState
      logger <- newLogger defaultLogSettings
      let env = mkEnv wbpsScheme pendingStore completeStore clientRegVar mockStateVar logger
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
        logger <- newLogger defaultLogSettings
        let env = mkEnv wbpsSchemeTmp pendingStore completeStore clientRegVar mockStateVar logger
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
      logger <- newLogger defaultLogSettings
      let env = mkEnv wbpsScheme pendingStore completeStore clientRegVar mockStateVar logger
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
      logger <- newLogger defaultLogSettings
      let env = mkEnv wbpsScheme pendingStore completeStore clientRegVar mockStateVar logger
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
      logger <- newLogger defaultLogSettings
      let env = mkEnv wbpsScheme pendingStore completeStore clientRegVar mockStateVar logger
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
      logger <- newLogger defaultLogSettings
      let env = mkEnv wbpsScheme pendingStore completeStore clientRegVar mockStateVar logger
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

        -- Register the client to the server
        registerResp <-
          runClientOrFail
            servantEnv
            ( registerClient
                Register.Inputs
                  { userPublicKey = Ed.toPublic testClientSecretKey
                  , xPublicKey = testWbpsPublicKey
                  }
            )

        let Register.Outputs {verificationContext = verificationPayload, id = registeredId} = registerResp

        case verificationPayload of
          Aeson.Object _ -> pure ()
          _ -> expectationFailure "verification context is not a JSON object"

        -- Test that the client was registered
        ClientsResp {clients = clientInfos} <- runClientOrFail servantEnv clientsClient
        clientInfos
          `shouldBe` [ClientInfo {clientId = registeredId, userPublicKey = expectedUserPublicKeyHex}]

        -- Ask the SP about pending transactions...there should be none
        PendingResp ps <-
          runClientOrFail servantEnv pendingClient
        ps `shouldBe` mempty

        -- Ask the SP to construct a transaction based on an intent
        prepareReq <- mkPrepareReqOrFail (ClientId registeredId) testIntentW
        DemonstrateCommitment.Outputs {txId, txAbs} <-
          runClientOrFail servantEnv (demonstrateCommitmentClient prepareReq)
        let unknownTxId =
              case Text.uncons txId of
                Nothing -> error "expected non-empty tx id"
                Just (c, rest) ->
                  let replacement = if c == '0' then '1' else '0'
                   in Text.cons replacement rest

        -- Ask the SP about a nonsense transaction ID, and expect a `TransactionMissing` response
        missingResp <-
          runClientOrFail servantEnv (transactionClient unknownTxId)
        missingResp `shouldBe` TransactionMissing

        -- Ask the SP about pending transactions...there should be 1
        PendingResp ps <-
          runClientOrFail servantEnv pendingClient
        length ps `shouldBe` 1

        -- Ask the SP about the in-flight transaction ID. It should be `Pending`.
        pendingTransactionResp <-
          runClientOrFail servantEnv (transactionClient txId)
        case (ps, pendingTransactionResp) of
          ( [ PendingItem
                { txId = pendingTxId
                , expiresAt = pendingExpiresAtExpected
                , clientId = pendingClientIdExpected
                }
              ]
            , TransactionPending PendingSummary {pendingExpiresAt, pendingClientId}
            ) -> do
              pendingTxId `shouldBe` txId
              pendingExpiresAt `shouldBe` pendingExpiresAtExpected
              pendingClientId `shouldBe` registeredId
              pendingClientIdExpected `shouldBe` registeredId
          (_, TransactionPending _) ->
            expectationFailure "expected exactly one pending item to compare with transaction response"
          (_, TransactionSubmitted _) ->
            expectationFailure "expected pending transaction, but it is already submitted"
          (_, TransactionMissing) ->
            expectationFailure "expected pending transaction, but it is missing"

        -- Commit with big R
        let commitReq = CommitReq {txId, bigR = Ed.toPublic testCommitSecretKey}
        _ <- runClientOrFail servantEnv (commitClient commitReq)

        -- Tell the SP to submit the transaction
        let finaliseReq = mkFinaliseReq testClientSecretKey txId (hashTxAbs txAbs)
        AskSubmission.Outputs {txId, submittedAt, result} <-
          runClientOrFail servantEnv (askSubmissionClient finaliseReq)
        result `shouldBe` AskSubmission.Finalised

        -- Ask the SP about pending transactions...there should be none again, since we submitted our transaction
        PendingResp pendingAfter <-
          runClientOrFail servantEnv pendingClient
        pendingAfter `shouldBe` mempty
        transactionResp <-
          runClientOrFail servantEnv (transactionClient txId)

        -- Ask the SP about the in-flight transaction ID. It should be `TransactionSubmitted`.
        case transactionResp of
          TransactionSubmitted SubmittedSummary {submittedTx, submittedAt = submittedAt', submittedClientId} -> do
            case submittedTx of
              CardanoEmulatorEraTx cardanoTx -> do
                let submittedTxId =
                      Api.serialiseToRawBytesHexText (Api.getTxId (Api.getTxBody cardanoTx))
                submittedTxId `shouldBe` txId
            submittedAt' `shouldBe` submittedAt
            submittedClientId `shouldBe` registeredId
          TransactionPending {} ->
            expectationFailure "expected submitted transaction, but it is still pending"
          TransactionMissing ->
            expectationFailure "expected submitted transaction, but it is missing"

mkPrepareReqOrFail :: ClientId -> IntentW -> IO Core.SP.DemonstrateCommitment.Inputs
mkPrepareReqOrFail cid iw =
  case Mock.mkPrepareReq cid iw of
    Left err -> expectationFailure (Text.unpack err) >> fail "invalid demonstrateCommitment request"
    Right req -> pure req

corruptSignature :: ByteString -> ByteString
corruptSignature bs =
  case BS.uncons bs of
    Nothing -> bs
    Just (b, rest) -> BS.cons (b `xor` 0x01) rest

expectSignerKey :: IO SignerKey
expectSignerKey =
  case Register.mkSignerKey (Ed.toPublic testClientSecretKey) of
    Nothing -> expectationFailure "expected valid signer key for test client" >> fail "invalid signer key"
    Just signerKey -> pure signerKey

expectedVerificationContext :: FileScheme -> IO Aeson.Value
expectedVerificationContext scheme = do
  signerKey <- expectSignerKey
  verificationPath <- verificationContextFilePath scheme signerKey
  decodeJsonFile verificationPath

verificationContextFilePath :: FileScheme -> SignerKey -> IO FilePath
verificationContextFilePath FileScheme {accounts, verificationContext} signerKey = do
  relDir <-
    case Path.parseRelDir (show signerKey) of
      Left err -> expectationFailure ("invalid account directory: " <> show err) >> fail "invalid account dir"
      Right dir -> pure dir
  pure $ Path.toFilePath (accounts </> relDir </> verificationContext)

decodeJsonFile :: FilePath -> IO Aeson.Value
decodeJsonFile filePath = do
  bytes <- BL.readFile filePath
  case Aeson.eitherDecode bytes of
    Left err ->
      expectationFailure ("failed to decode verification context JSON: " <> err)
        >> fail "invalid verification context file"
    Right value -> pure value

setupMinimalWbpsTree :: FilePath -> IO ()
setupMinimalWbpsTree root = do
  let relationDir = root FP.</> "inputs" FP.</> "relation"
      relationFile = relationDir FP.</> "relation.r1cs"
      setupDir = root FP.</> "inputs" FP.</> "setup"
      ptauFile = setupDir FP.</> "powersOfTauPrepared.ptau"
  createDirectoryIfMissing True relationDir
  writeFile relationFile ""
  createDirectoryIfMissing True setupDir
  writeFile ptauFile ""
  createDirectoryIfMissing True (root FP.</> "output" FP.</> "accounts")

accountDirectoryPath :: FilePath -> SignerKey -> IO FilePath
accountDirectoryPath root signerKey =
  pure (root FP.</> "output" FP.</> "accounts" FP.</> show signerKey)
