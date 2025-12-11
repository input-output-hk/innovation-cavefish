{-# LANGUAGE OverloadedStrings #-}

module Core.CborSpec () where

-- import Cardano.Api qualified as Api
-- import Client.Mock qualified as Mock
-- import Control.Concurrent.STM (newTVarIO)
-- import Cooked (Wallet, wallet)
-- import Core.Api.Config (Config)
-- import Core.Cbor (maskTxBody, serialiseTxBody)
-- import Core.Intent (
--   AddressW (AddressW),
--   BuildTxResult (BuildTxResult, tx),
--   IntentDSL (AndExpsW, PayToW, SpendFromW),
--   toInternalIntent,
--  )
-- import Core.SP.DemonstrateCommitment qualified as DemonstrateCommitment
-- import Data.Default (def)
-- import Data.List.NonEmpty (NonEmpty ((:|)))
-- import Data.Map.Strict qualified as Map
-- import Data.Text (Text)
-- import Data.Text qualified as T
-- import Ledger.Tx.CardanoAPI (toCardanoAddressInEra)
-- import Plutus.Script.Utils.Address qualified as ScriptAddr
-- import Sp.Emulator (buildWithCooked, initialMockState, mkServerContext)
-- import Test.Hspec (Spec, describe, expectationFailure, it)
-- import WBPS.Core.FileScheme (mkFileSchemeFromRoot)
-- import WBPS.Core.Keys.Ed25519 (userWalletPK)
-- import WBPS.Core.Keys.Ed25519 qualified as Ed25519

-- testNetworkId :: Api.NetworkId
-- testNetworkId = Api.Testnet (Api.NetworkMagic 1)

-- testPayToWallet :: Wallet
-- testPayToWallet = wallet 3

-- testSpendFromWallet :: Wallet
-- testSpendFromWallet = wallet 2

-- testIntentW :: IntentDSL
-- testIntentW =
--   AndExpsW
--     ( PayToW testPayToValue (AddressW testPayToAddressText)
--         :| [SpendFromW (AddressW testSpendFromAddressText)]
--     )

-- testPayToAddressText :: Text
-- testPayToAddressText = Api.serialiseAddress testPayToAddress

-- testPayToAddress :: Api.AddressInEra Api.ConwayEra
-- testPayToAddress =
--   case toCardanoAddressInEra testNetworkId (ScriptAddr.toAddress testPayToWallet) of
--     Left err -> error ("failed to derive pay-to address: " <> show err)
--     Right addr -> addr

-- testSpendFromAddressText :: Text
-- testSpendFromAddressText = Api.serialiseAddress testSpendFromAddress

-- testSpendFromAddress :: Api.AddressInEra Api.ConwayEra
-- testSpendFromAddress =
--   case toCardanoAddressInEra testNetworkId (ScriptAddr.toAddress testSpendFromWallet) of
--     Left err -> error ("failed to derive spend-from address: " <> show err)
--     Right addr -> addr

-- testPayToValue :: Api.Value
-- testPayToValue = Api.lovelaceToValue 2

-- spec :: Spec
-- spec =
--   describe "maskTxBody" $
--     it "produces a masked body that still deserialises as a Conway TxBody" $ do
--       mockState <- newTVarIO initialMockState
--       pendingStore <- newTVarIO Map.empty
--       completeStore <- newTVarIO Map.empty
--       keyPair <- Ed25519.generateKeyPair
--       wbpsScheme <- mkFileSchemeFromRoot "../../wbps"
--       let config :: Config = def
--       let env =
--             mkServerContext
--               mockState
--               pendingStore
--               completeStore
--               wbpsScheme
--               config

--       prepareReq <-
--         case Mock.mkDemonstrateCommitmentInputs (userWalletPK keyPair) testIntentW of
--           Left err -> expectationFailure (T.unpack err) >> fail "invalid demonstrateCommitment request"
--           Right req -> pure req

--       intent <-
--         case toInternalIntent testIntentW of
--           Left err -> expectationFailure (T.unpack err) >> fail "invalid intent"
--           Right v -> pure v

--       -- let observerBytes = DemonstrateCommitment.observer prepareReq

--       -- buildResult <- buildWithCooked mockState env intent observerBytes
--       let BuildTxResult {tx = builtTx} = buildResult
--           rawBody = serialiseTxBody builtTx
--       case maskTxBody rawBody of
--         Left err -> expectationFailure (T.unpack err) >> fail "maskTxBody failed"
--         Right masked ->
--           case Api.deserialiseFromCBOR (Api.AsTxBody Api.AsConwayEra) masked of
--             Left parseErr ->
--               expectationFailure ("masked body failed to deserialise as a Conway TxBody: " <> show parseErr)
--             Right (_ :: Api.TxBody Api.ConwayEra) -> pure ()
