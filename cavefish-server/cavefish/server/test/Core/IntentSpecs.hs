{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Use tuple-section" -}

module Core.IntentSpecs (tests) where

import Cardano.Api (lovelaceToValue, serialiseAddress)
import Cardano.Api qualified as Api
import Cavefish.Services.TxBuilding (ServiceFee (ServiceFee, amount, paidTo))
import Cooked qualified
import Core.IntentDSLGen (genDSL)
import Data.Either (isLeft)
import Data.List.NonEmpty (fromList)
import Data.Text qualified as Text (unpack)
import Hedgehog (Property, assert, footnote, forAll, property)
import Intent.Example.DSL (
  AddressW (AddressW),
  CanonicalIntent (payTo),
  IntentDSL (AndExpsW, ChangeToW, MaxIntervalW, MustMintW, PayToW, SpendFromW),
  satisfies,
  toCanonicalIntent,
  unAdressConwayEra,
 )
import Ledger.CardanoWallet qualified
import Plutus.Script.Utils.Value qualified as Script
import Sp.Emulator (buildWithCooked)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)
import Test.Tasty.Hspec (testSpec)
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (PaymentAddess (PaymentAddess), Wallet (Wallet, paymentAddress))
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 qualified as Ed25519
import WBPS.Core.Session.Demonstration.Artefacts.Cardano.UnsignedTx (AbstractUnsignedTx (AbstractUnsignedTx))

tests :: IO ()
tests = do
  spec <- testSpec "spec" specs
  defaultMain $ testGroup "IntentDSL" [spec, properties]

properties :: TestTree
properties =
  testGroup
    "IntentDSL property tests"
    [ testPropertyNamed "Property test, convert IntentDSL to CanonicalIntent" "propIntentDSL" propIntentDSL
    ]

propIntentDSL :: Property
propIntentDSL = property $ do
  dsl <- forAll genDSL
  let
    alice = (Cooked.wallet 1, [Script.ada 110, Script.ada 200])
    mary = (Cooked.wallet 1, [Script.ada 110, Script.ada 200])
    bob = (Cooked.wallet 1, [Script.ada 110, Script.ada 200])
    provider = (Cooked.wallet 1, [Script.ada 110, Script.ada 200])

    initialDistribution -- found all wallets
      =
      Cooked.distributionFromList [alice, mary, bob, provider]

  let paymentAddress =
        Ed25519.PaymentAddess
          . Api.serialiseAddress
          . Ledger.CardanoWallet.mockWalletAddress
          $ Cooked.wallet 6
  let servicefee = ServiceFee {amount = 7, paidTo = paymentAddress}

  let intent = toCanonicalIntent dsl
  case intent of
    Left err -> do
      footnote ("IntentDSL to CanonicalIntent conversion Error: " <> Text.unpack err)
      assert False
    Right _ -> do
      absTx <- buildWithCooked initialDistribution servicefee dsl
      assert $ satisfies dsl (AbstractUnsignedTx absTx)

specs :: Spec
specs =
  describe "toCanonicalIntent" $ do
    it "converts an intent DSL into a canonical intent using a generated Ed25519 address" $ do
      Wallet {paymentAddress = (PaymentAddess expectedPaymentVerificationKey)} <- Ed25519.generateWallet
      let changeTo = AddressW . Api.serialiseAddress $ Ledger.CardanoWallet.mockWalletAddress $ Cooked.wallet 1

      case toCanonicalIntent
        ( AndExpsW $
            fromList
              [ PayToW (lovelaceToValue 10_000_000) (AddressW expectedPaymentVerificationKey)
              , ChangeToW changeTo
              ]
        ) of
        Left err -> expectationFailure (Text.unpack err)
        Right canonicalIntent ->
          case payTo canonicalIntent of
            [(outValue, outAddr)] -> do
              outValue `shouldBe` lovelaceToValue 10_000_000
              serialiseAddress (unAdressConwayEra outAddr) `shouldBe` expectedPaymentVerificationKey
            other -> expectationFailure ("unexpected payTo entries: " <> show other)

    it "conversion fails when the IntentDSL is invalid" $ do
      toCanonicalIntent
        ( AndExpsW $
            fromList
              [ MustMintW (lovelaceToValue 10_000_000)
              , SpendFromW (AddressW "address-123")
              , MaxIntervalW 10
              ]
        )
        `shouldSatisfy` Data.Either.isLeft
