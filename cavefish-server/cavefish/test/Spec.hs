{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec (spec) where

import qualified Cardano.Api as Api
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
import qualified Plutus.Script.Utils.Address as ScriptAddr
import Servant
import Sp.App (Env (..), runApp)
import Sp.Emulator (buildWithCooked, initialMockState, mkCookedEnv)
import Sp.Server (
  ClientInfo (..),
  ClientsResp (..),
  FinaliseReq (..),
  FinaliseResp (..),
  FinaliseResult (..),
  PendingItem (..),
  PendingResp (..),
  PrepareReq (..),
  PrepareResp (..),
  finaliseH,
  hashTxAbs,
 )
import Sp.State (ClientId (..), ClientRegistrationStore, Pending (..), PendingStore)
import Test.Hspec

runHandlerOrFail :: Handler a -> IO a
runHandlerOrFail handler = do
  result <- runExceptT (runHandler' handler)
  either (fail . show) pure result

mkEnv :: PendingStore -> ClientRegistrationStore -> TVar MockChainState -> Env
mkEnv pendingStore clientRegVar mockStateVar =
  mkCookedEnv mockStateVar pendingStore clientRegVar testSecretKey testSpWallet 3600 0

spec :: Spec
spec = do
  describe "buildTx integration" $ do
    it "prepare -> finalise roundtrip uses buildTx" $ do
      pendingStore <- newTVarIO Map.empty
      clientRegVar <- newTVarIO Map.empty
      mockStateVar <- newTVarIO initialMockState
      let env = mkEnv pendingStore clientRegVar mockStateVar
      let mockClient0 = Mock.initMockClient (runApp env) testClientSecretKey (Ed.toPublic testSecretKey)
      mockClient <- runHandlerOrFail (Mock.register mockClient0)
      registeredClientId <- mockClientIdOrFail mockClient
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
      prepareResp@PrepareResp{txId = gotTxId, txAbs = gotTxAbs, proof = gotProof} <-
        runHandlerOrFail (Mock.prepareAndVerifyWithClient mockClient testIntentW)
      gotTxId `shouldBe` expectedTxId
      gotTxAbs `shouldBe` expectedTxAbs
      gotProof `shouldBe` expectedProof
      satisfies expectedDelta internalIntent gotTxAbs `shouldBe` True
      pendingMap <- readTVarIO pendingStore
      case Map.lookup expectedTxIdValue pendingMap of
        Nothing -> expectationFailure "pending entry not stored"
        Just Pending{tx, txAbsHash} -> do
          tx `shouldBe` expectedTx
          txAbsHash `shouldBe` expectedTxAbsHash
          let storedTxAbs = cardanoTxToTxAbs (CardanoEmulatorEraTx tx)
          satisfies expectedDelta internalIntent storedTxAbs `shouldBe` True
          let storedProof = mkProof (spSk env) expectedTxIdValue txAbsHash
          gotProof `shouldBe` storedProof
      FinaliseResp{txId = finalTxId, result = finalResult} <-
        runHandlerOrFail (Mock.finaliseWithClient mockClient prepareResp)
      pendingAfter <- readTVarIO pendingStore
      Map.notMember expectedTxIdValue pendingAfter `shouldBe` True
      finalTxId `shouldBe` gotTxId
      finalResult `shouldBe` Finalised

    it "rejects finalise with invalid signature" $ do
      pendingStore <- newTVarIO Map.empty
      clientRegVar <- newTVarIO Map.empty
      mockStateVar <- newTVarIO initialMockState
      let env = mkEnv pendingStore clientRegVar mockStateVar
      let mockClient0 = Mock.initMockClient (runApp env) testClientSecretKey (Ed.toPublic testSecretKey)
      mockClient <- runHandlerOrFail (Mock.register mockClient0)
      registeredClientId <- mockClientIdOrFail mockClient
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
      clientRegVar <- newTVarIO Map.empty
      mockStateVar <- newTVarIO initialMockState
      let env = mkEnv pendingStore clientRegVar mockStateVar
          expectedPublicKey = renderHex (BA.convert (Ed.toPublic testClientSecretKey))
      let mockClient0 = Mock.initMockClient (runApp env) testClientSecretKey (Ed.toPublic testSecretKey)
      mockClient <- runHandlerOrFail (Mock.register mockClient0)
      ClientId clientUuid <- mockClientIdOrFail mockClient
      ClientsResp{clients = clientInfos} <-
        runHandlerOrFail (Mock.getClientsWithClient mockClient)
      clientInfos
        `shouldBe` [ClientInfo{clientId = clientUuid, publicKey = expectedPublicKey}]

    it "pending endpoint returns stored pending transactions" $ do
      pendingStore <- newTVarIO Map.empty
      clientRegVar <- newTVarIO Map.empty
      mockStateVar <- newTVarIO initialMockState
      let env = mkEnv pendingStore clientRegVar mockStateVar
      let mockClient0 = Mock.initMockClient (runApp env) testClientSecretKey (Ed.toPublic testSecretKey)
      mockClient <- runHandlerOrFail (Mock.register mockClient0)
      ClientId clientUuid <- mockClientIdOrFail mockClient
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

mockClientIdOrFail :: Mock.MockClient -> IO ClientId
mockClientIdOrFail mockClient =
  case mockClient.mcClientId of
    Nothing -> expectationFailure "mock client not registered" >> fail "mock client not registered"
    Just cid -> pure cid

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
