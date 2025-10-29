{-# LANGUAGE OverloadedStrings #-}

module Core.CborSpec (spec) where

import Cardano.Api qualified as Api
import Client.Mock qualified as Mock
import Control.Concurrent.STM (newTVarIO)
import Core.Api.Messages (PrepareReq (..))
import Core.Api.State (ClientId (..))
import Core.Cbor (maskTxBody, serialiseTxBody)
import Core.Intent (BuildTxResult (..), toInternalIntent)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.UUID qualified as UUID
import Sp.Emulator (buildWithCooked, initialMockState, mkCookedEnv)
import Test.Common
import Test.Hspec

spec :: Spec
spec =
  describe "maskTxBody" $
    it "produces a masked body that still deserialises as a Conway TxBody" $ do
      mockState <- newTVarIO initialMockState
      pendingStore <- newTVarIO Map.empty
      completeStore <- newTVarIO Map.empty
      clientStore <- newTVarIO Map.empty

      let env =
            mkCookedEnv
              mockState
              pendingStore
              completeStore
              clientStore
              testSecretKey
              testPkeSecretKey
              testSpWallet
              3600
              0

      prepareReq <-
        case Mock.mkPrepareReq (ClientId UUID.nil) testIntentW of
          Left err -> expectationFailure (T.unpack err) >> fail "invalid prepare request"
          Right req -> pure req

      intent <-
        case toInternalIntent testIntentW of
          Left err -> expectationFailure (T.unpack err) >> fail "invalid intent"
          Right v -> pure v

      let observerBytes = observer prepareReq

      buildResult <- buildWithCooked mockState env intent observerBytes
      let BuildTxResult {tx = builtTx} = buildResult
          rawBody = serialiseTxBody builtTx
      case maskTxBody rawBody of
        Left err -> expectationFailure (T.unpack err) >> fail "maskTxBody failed"
        Right masked ->
          case Api.deserialiseFromCBOR (Api.AsTxBody Api.AsConwayEra) masked of
            Left parseErr ->
              expectationFailure ("masked body failed to deserialise as a Conway TxBody: " <> show parseErr)
            Right (_ :: Api.TxBody Api.ConwayEra) -> pure ()
