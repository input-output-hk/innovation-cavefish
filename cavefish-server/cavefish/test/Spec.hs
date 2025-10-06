{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec (spec) where

import qualified Cardano.Api as Api
import qualified Client.Impl as Client
import Client.Mock (mkFinaliseReq)
import qualified Client.Mock as Mock
import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO)
import Control.Monad.Trans.Except (runExceptT)
import Cooked (Wallet, wallet)
import Cooked.MockChain.MockChainState (MockChainState)
import Core.Intent (AddressW (..), BuildTxResult (..), IntentW (..), satisfies, toInternalIntent)
import Core.Proof (mkProof, renderHex)
import Core.TxAbs (cardanoTxToTxAbs)
import Crypto.Error (CryptoFailable (..))
import qualified Crypto.PubKey.Ed25519 as Ed
import Data.Bits (xor)
import qualified Data.ByteArray as BA
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Ledger.Tx (
  pattern CardanoEmulatorEraTx,
 )
import Ledger.Tx.CardanoAPI (toCardanoAddressInEra)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Plutus.Script.Utils.Address as ScriptAddr
import Servant
import Servant.Client (BaseUrl (..), Scheme (..))
import qualified Servant.Client as SC
import Sp.App (Env (..), runApp)
import Sp.Emulator (buildWithCooked, initialMockState, mkCookedEnv)
import Sp.Server (CavefishApi, ClientInfo (..), ClientsResp (..), FinaliseReq (..), FinaliseResp (..), FinaliseResult (..), PendingItem (..), PendingResp (..), PendingSummary (..), PrepareReq (..), PrepareResp (..), RegisterReq (..), RegisterResp (..), SubmittedSummary (..), TransactionResp (..), finaliseH, hashTxAbs, mkApp, transactionH)
import Sp.State (ClientId (..), ClientRegistrationStore, CompleteStore, Pending (..), PendingStore)
import Test.Hspec

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

mkEnv :: PendingStore -> CompleteStore -> ClientRegistrationStore -> TVar MockChainState -> Env
mkEnv pendingStore completeStore clientRegVar mockStateVar =
  mkCookedEnv mockStateVar pendingStore completeStore clientRegVar testSecretKey testSpWallet 3600 0

mkClientEnv :: Env -> Client.ClientEnv
mkClientEnv env =
  Client.ClientEnv
    { Client.run = runApp env
    , Client.lcSk = testClientSecretKey
    , Client.spPk = Ed.toPublic testSecretKey
    }

spec :: Spec
spec = do
  describe "buildTx integration" $ do
    it "prepare -> finalise roundtrip uses buildTx" $ do
      pendingStore <- newTVarIO Map.empty
      completeStore <- newTVarIO Map.empty
      clientRegVar <- newTVarIO Map.empty
      mockStateVar <- newTVarIO initialMockState
      let env = mkEnv pendingStore completeStore clientRegVar mockStateVar
      let mockClient0 = Mock.initMockClient (runApp env) testClientSecretKey (Ed.toPublic testSecretKey)
      mockClient <- runHandlerOrFail (Mock.register mockClient0)
      let registeredClientId = mockClient.mcClientId
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
          expectedProof = mkProof (spSk env) expectedTxIdValue expectedTxAbsHash
          fetchTransaction txIdTxt =
            runExceptT (runHandler' (runApp env (transactionH txIdTxt)))
          ClientId expectedClientUuid = registeredClientId

      beforeExists <- fetchTransaction expectedTxId
      case beforeExists of
        Right TransactionMissing -> pure ()
        Right resp -> expectationFailure ("expected missing transaction but got " <> show resp)
        Left err -> expectationFailure ("expected missing transaction but got error: " <> show err)

      prepareResp@PrepareResp{txId = gotTxId, txAbs = gotTxAbs, proof = gotProof} <-
        runHandlerOrFail (Mock.prepareAndVerifyWithClient mockClient testIntentW)
      gotTxId `shouldBe` expectedTxId
      gotTxAbs `shouldBe` expectedTxAbs
      gotProof `shouldBe` expectedProof
      satisfies expectedDelta internalIntent gotTxAbs `shouldBe` True
      pendingMap <- readTVarIO pendingStore
      pendingEntry <-
        case Map.lookup expectedTxIdValue pendingMap of
          Nothing -> expectationFailure "pending entry not stored" >> fail "missing pending entry"
          Just entry -> pure entry
      let Pending{tx = storedTx, txAbsHash = storedTxAbsHash, expiry = storedExpiry, creator = storedCreator} = pendingEntry
      storedTx `shouldBe` expectedTx
      storedTxAbsHash `shouldBe` expectedTxAbsHash
      let storedTxAbs = cardanoTxToTxAbs (CardanoEmulatorEraTx storedTx)
      satisfies expectedDelta internalIntent storedTxAbs `shouldBe` True
      let storedProof = mkProof (spSk env) expectedTxIdValue storedTxAbsHash
      gotProof `shouldBe` storedProof
      storedCreator `shouldBe` registeredClientId

      pendingStatus <- fetchTransaction expectedTxId
      case pendingStatus of
        Right (TransactionPending PendingSummary{pendingExpiresAt, pendingClientId}) -> do
          pendingExpiresAt `shouldBe` storedExpiry
          pendingClientId `shouldBe` expectedClientUuid
        Right resp -> expectationFailure ("expected pending transaction but got " <> show resp)
        Left err -> expectationFailure ("expected pending transaction but got error: " <> show err)

      FinaliseResp{txId = finalTxId, result = finalResult, submittedAt = finalSubmittedAt} <-
        runHandlerOrFail (Mock.finaliseWithClient mockClient prepareResp)
      pendingAfter <- readTVarIO pendingStore
      Map.notMember expectedTxIdValue pendingAfter `shouldBe` True
      finalTxId `shouldBe` gotTxId
      finalResult `shouldBe` Finalised

      submittedStatus <- fetchTransaction expectedTxId
      case submittedStatus of
        Right (TransactionSubmitted SubmittedSummary{submittedTx, submittedAt, submittedClientId}) -> do
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
      let env = mkEnv pendingStore completeStore clientRegVar mockStateVar
      let mockClient0 = Mock.initMockClient (runApp env) testClientSecretKey (Ed.toPublic testSecretKey)
      mockClient <- runHandlerOrFail (Mock.register mockClient0)
      let registeredClientId = mockClient.mcClientId
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
      _ <- runHandlerOrFail (Mock.prepareAndVerifyWithClient mockClient testIntentW)
      pendingMap <- readTVarIO pendingStore
      Map.member expectedTxIdValue pendingMap `shouldBe` True
      let validFinaliseReq = Mock.mkFinaliseReq mockClient.mcLcSk expectedTxId expectedTxAbsHash
          invalidFinaliseReq = validFinaliseReq{lcSig = corruptSignature validFinaliseReq.lcSig}
      FinaliseResp{txId = finalTxId, result = finalResult} <-
        runHandlerOrFail (runApp env $ finaliseH invalidFinaliseReq)
      finalTxId `shouldBe` expectedTxId
      finalResult `shouldBe` Rejected "invalid client signature"
      pendingAfter <- readTVarIO pendingStore
      Map.member expectedTxIdValue pendingAfter `shouldBe` True

  describe "API surfaces" $ do
    it "clients endpoint returns registered clients" $ do
      pendingStore <- newTVarIO Map.empty
      completeStore <- newTVarIO Map.empty
      clientRegVar <- newTVarIO Map.empty
      mockStateVar <- newTVarIO initialMockState
      let env = mkEnv pendingStore completeStore clientRegVar mockStateVar
          expectedPublicKey = renderHex (BA.convert (Ed.toPublic testClientSecretKey))
      let mockClient0 = Mock.initMockClient (runApp env) testClientSecretKey (Ed.toPublic testSecretKey)
      mockClient <- runHandlerOrFail (Mock.register mockClient0)
      let ClientId clientUuid = mockClient.mcClientId
      ClientsResp{clients = clientInfos} <-
        runHandlerOrFail (Mock.getClientsWithClient mockClient)
      clientInfos
        `shouldBe` [ClientInfo{clientId = clientUuid, publicKey = expectedPublicKey}]

    it "pending endpoint returns stored pending transactions" $ do
      pendingStore <- newTVarIO Map.empty
      completeStore <- newTVarIO Map.empty
      clientRegVar <- newTVarIO Map.empty
      mockStateVar <- newTVarIO initialMockState
      let env = mkEnv pendingStore completeStore clientRegVar mockStateVar
      let mockClient0 = Mock.initMockClient (runApp env) testClientSecretKey (Ed.toPublic testSecretKey)
      mockClient <- runHandlerOrFail (Mock.register mockClient0)
      let ClientId clientUuid = mockClient.mcClientId
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
      _ <- runHandlerOrFail (Mock.prepareAndVerifyWithClient mockClient testIntentW)
      pendingMap <- readTVarIO pendingStore
      case Map.lookup expectedTxIdValue pendingMap of
        Nothing -> expectationFailure "pending entry not stored"
        Just Pending{expiry = expectedExpiry, creator} -> do
          creator `shouldBe` registeredClientId
          let expectedItem =
                PendingItem
                  { txId = expectedTxId
                  , txAbsHash = renderHex expectedTxAbsHash
                  , expiresAt = expectedExpiry
                  , clientId = clientUuid
                  }
          PendingResp{pending = items} <-
            runHandlerOrFail (Mock.getPendingWithClient mockClient)
          items `shouldBe` [expectedItem]

  describe "Client implementation" $ do
    it "runIntent performs prepare/verify/finalise" $ do
      pendingStore <- newTVarIO Map.empty
      completeStore <- newTVarIO Map.empty
      clientRegVar <- newTVarIO Map.empty
      mockStateVar <- newTVarIO initialMockState
      let env = mkEnv pendingStore completeStore clientRegVar mockStateVar
          clientEnv = mkClientEnv env
      FinaliseResp{result = finalResult} <-
        runHandlerOrFail $
          Client.withSession clientEnv $
            \session -> Client.runIntent session testIntentW
      finalResult `shouldBe` Finalised
      PendingResp{pending = pendingAfter} <-
        runHandlerOrFail (Client.runClient clientEnv Client.listPending)
      pendingAfter `shouldBe` []
      let expectedPublicKey = renderHex (BA.convert (Ed.toPublic testClientSecretKey))
      ClientsResp{clients = clientInfos} <-
        runHandlerOrFail (Client.runClient clientEnv Client.listClients)
      case clientInfos of
        [ClientInfo{publicKey}] -> publicKey `shouldBe` expectedPublicKey
        _ -> expectationFailure "expected exactly one registered client"

  describe "Http server roundtrip" $ do
    it "prepare -> finalise roundtrip uses buildTx" $ do
      pendingStore <- newTVarIO Map.empty
      completeStore <- newTVarIO Map.empty
      clientRegVar <- newTVarIO Map.empty
      mockStateVar <- newTVarIO initialMockState
      let env = mkEnv pendingStore completeStore clientRegVar mockStateVar
      let application = pure (mkApp env)
      Warp.testWithApplication application $ \port -> do
        manager <- newManager defaultManagerSettings
        let baseUrl = BaseUrl Http "127.0.0.1" port ""
            servantEnv = SC.mkClientEnv manager baseUrl
            (prepareClient :<|> finaliseClient :<|> registerClient :<|> clientsClient :<|> pendingClient :<|> transactionClient) =
              SC.client (Proxy @CavefishApi)

        -- Register the client to the server
        registerResp <-
          runClientOrFail servantEnv (registerClient RegisterReq{publicKey = Ed.toPublic testClientSecretKey})
        let expectedPublicKey = renderHex (BA.convert (Ed.toPublic testClientSecretKey))

        -- Test that the client was registered
        ClientsResp{clients = clientInfos} <- runClientOrFail servantEnv clientsClient
        clientInfos
          `shouldBe` [ClientInfo{clientId = registerResp.id, publicKey = expectedPublicKey}]

        -- Ask the SP about pending transactions...there should be none
        PendingResp ps <-
          runClientOrFail servantEnv pendingClient
        ps `shouldBe` mempty

        -- Ask the SP to construct a transaction based on an intent
        prepareReq <- mkPrepareReqOrFail (ClientId registerResp.id) testIntentW
        PrepareResp{txId, txAbs, proof, changeDelta} <-
          runClientOrFail servantEnv (prepareClient prepareReq)
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
          ([PendingItem{txId = pendingTxId, expiresAt = pendingExpiresAtExpected, clientId = pendingClientIdExpected}], TransactionPending PendingSummary{pendingExpiresAt, pendingClientId}) -> do
            pendingTxId `shouldBe` txId
            pendingExpiresAt `shouldBe` pendingExpiresAtExpected
            pendingClientId `shouldBe` registerResp.id
            pendingClientIdExpected `shouldBe` registerResp.id
          (_, TransactionPending _) ->
            expectationFailure "expected exactly one pending item to compare with transaction response"
          (_, TransactionSubmitted _) ->
            expectationFailure "expected pending transaction, but it is already submitted"
          (_, TransactionMissing) ->
            expectationFailure "expected pending transaction, but it is missing"

        -- Tell the SP to submit the transaction
        let finaliseReq = mkFinaliseReq testClientSecretKey txId (hashTxAbs txAbs)
        finaliseResp@FinaliseResp{txId, submittedAt, result} <-
          runClientOrFail servantEnv (finaliseClient finaliseReq)
        result `shouldBe` Finalised

        -- Ask the SP about pending transactions...there should be none again, since we submitted our transaction
        PendingResp pendingAfter <-
          runClientOrFail servantEnv pendingClient
        pendingAfter `shouldBe` mempty
        transactionResp <-
          runClientOrFail servantEnv (transactionClient txId)

        -- Ask the SP about the in-flight transaction ID. It should be `TransactionSubmitted`.
        case transactionResp of
          TransactionSubmitted SubmittedSummary{submittedTx, submittedAt = submittedAt', submittedClientId} -> do
            case submittedTx of
              CardanoEmulatorEraTx cardanoTx -> do
                let submittedTxId =
                      Api.serialiseToRawBytesHexText (Api.getTxId (Api.getTxBody cardanoTx))
                submittedTxId `shouldBe` txId
            submittedAt' `shouldBe` submittedAt
            submittedClientId `shouldBe` registerResp.id
          TransactionPending{} ->
            expectationFailure "expected submitted transaction, but it is still pending"
          TransactionMissing ->
            expectationFailure "expected submitted transaction, but it is missing"

testIntentW :: IntentW
testIntentW =
  AndExpsW
    ( PayToW testPayToValue (AddressW testPayToAddressText)
        :| [SpendFromW (AddressW testSpendFromAddressText)]
    )

mkPrepareReqOrFail :: ClientId -> IntentW -> IO PrepareReq
mkPrepareReqOrFail cid iw =
  case Mock.mkPrepareReq cid iw of
    Left err -> expectationFailure (Text.unpack err) >> fail "invalid prepare request"
    Right req -> pure req

corruptSignature :: ByteString -> ByteString
corruptSignature bs =
  case BS.uncons bs of
    Nothing -> bs
    Just (b, rest) -> BS.cons (b `xor` 0x01) rest

testNetworkId :: Api.NetworkId
testNetworkId = Api.Testnet (Api.NetworkMagic 1)

testPayToWallet :: Wallet
testPayToWallet = wallet 3

testPayToAddress :: Api.AddressInEra Api.ConwayEra
testPayToAddress =
  case toCardanoAddressInEra testNetworkId (ScriptAddr.toAddress testPayToWallet) of
    Left err -> error ("failed to derive pay-to address: " <> show err)
    Right addr -> addr

testPayToAddressText :: Text
testPayToAddressText = Api.serialiseAddress testPayToAddress

testPayToValue :: Api.Value
testPayToValue = Api.lovelaceToValue 2

testSpendFromWallet :: Wallet
testSpendFromWallet = wallet 2

testSpendFromAddress :: Api.AddressInEra Api.ConwayEra
testSpendFromAddress =
  case toCardanoAddressInEra testNetworkId (ScriptAddr.toAddress testSpendFromWallet) of
    Left err -> error ("failed to derive pay-to address: " <> show err)
    Right addr -> addr

testSpendFromAddressText :: Text
testSpendFromAddressText = Api.serialiseAddress testSpendFromAddress

testSpWallet :: Wallet
testSpWallet = wallet 1

testClientSecretKey :: Ed.SecretKey
testClientSecretKey =
  case Ed.secretKey (BS.pack [101 .. 132]) of
    CryptoPassed sk -> sk
    CryptoFailed err -> error ("invalid client secret key: " <> show err)

testSecretKey :: Ed.SecretKey
testSecretKey =
  case Ed.secretKey (BS.pack [1 .. 32]) of
    CryptoPassed sk -> sk
    CryptoFailed err -> error ("invalid static secret key: " <> show err)
