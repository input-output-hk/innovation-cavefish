{-# LANGUAGE OverloadedStrings #-}

{- HLINT ignore "Use tuple-section" -}

module Core.IntentSpecs (tests) where

import Cardano.Api (lovelaceToValue, serialiseAddress)
import Cavefish.Services.TxBuilding (ServiceFee (ServiceFee, amount, paidTo))
import Control.Monad.IO.Class (MonadIO (..))
import Cooked (knownWallets)
import Cooked.MockChain.BlockChain
import Core.IntentDSLGen (distributionFromList, genDSL)
import Data.Default
import Data.Either (isLeft)
import Data.List.NonEmpty (fromList)
import Data.Text qualified as Text (unpack)
import Hedgehog (Property, assert, footnote, forAll, property, (===))
import Intent.Example.DSL (
  AddressW (AddressW),
  CanonicalIntent (payTo),
  IntentDSL (AndExpsW, MaxIntervalW, MustMintW, PayToW, SpendFromW),
  satisfies,
  toCanonicalIntent,
  unAdressConwayEra,
 )
import Plutus.Script.Utils.Value (ada)
import Sp.Emulator
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)
import Test.Tasty.Hspec (testSpec)
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (KeyPair, PaymentAddess (PaymentAddess), Wallet (Wallet, paymentAddress), generateWallet)
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 qualified as Ed25519 (generateWallet)
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
  (dsl, wallets) <- forAll genDSL
  liftIO $ putStrLn "++++++++++++++++++++++++"
  liftIO $ print wallets
  liftIO $ putStrLn "$$$$$$$$$$$$$$$$$$$$$$$$"
  let
    initDistList = distributionFromList (fmap (\w -> (w, [ada 100_000_000])) wallets)
  provider@Wallet {paymentAddress} <- generateWallet
  let servicefee = ServiceFee {amount = 10, paidTo = paymentAddress}
  let intent = toCanonicalIntent dsl
  case intent of
    Left err -> do
      footnote ("IntentDSL to CanonicalIntent conversion Error: " <> Text.unpack err)
      assert False
    Right _ -> do
      absTx <- buildWithCooked initDistList servicefee dsl
      assert $ satisfies dsl (AbstractUnsignedTx absTx)

specs :: Spec
specs =
  describe "toCanonicalIntent" $ do
    it "converts an intent DSL into a canonical intent using a generated Ed25519 address" $ do
      Wallet {paymentAddress = (PaymentAddess expectedPaymentVerificationKey)} <- Ed25519.generateWallet

      case toCanonicalIntent
        ( AndExpsW $
            fromList
              [ PayToW (lovelaceToValue 10_000_000) (AddressW expectedPaymentVerificationKey)
              ]
        ) of
        Left err -> expectationFailure (Text.unpack err)
        Right canonicalIntent ->
          case payTo canonicalIntent of
            [(outValue, outAddr)] -> do
              print canonicalIntent
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
        `shouldSatisfy` isLeft
