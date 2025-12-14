{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Core.IntentSpecs (spec) where

import Cardano.Api (lovelaceToValue)
import Cardano.Api qualified as Api
import Core.Intent (
  AddressW (AddressW),
  CanonicalIntent (payTo),
  IntentDSL (PayToW),
  source,
  toCanonicalIntent,
 )
import Data.Text qualified as Text
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import WBPS.Core.Keys.Ed25519 (PaymentAddess (..), Wallet (..))
import WBPS.Core.Keys.Ed25519 qualified as Ed25519

spec :: Spec
spec =
  describe "toCanonicalIntent" $
    it "converts an intent DSL into a canonical intent using a generated Ed25519 address" $ do
      Wallet {paymentAddress = (PaymentAddess expectedPaymentVerificationKey)} <- Ed25519.generateWallet

      case toCanonicalIntent (PayToW (lovelaceToValue 10_000_000) (AddressW expectedPaymentVerificationKey)) of
        Left err -> expectationFailure (Text.unpack err)
        Right canonicalIntent ->
          case payTo canonicalIntent of
            [(outValue, outAddr)] -> do
              outValue `shouldBe` lovelaceToValue 10_000_000
              Api.serialiseAddress (source outAddr) `shouldBe` expectedPaymentVerificationKey
            other -> expectationFailure ("unexpected payTo entries: " <> show other)
