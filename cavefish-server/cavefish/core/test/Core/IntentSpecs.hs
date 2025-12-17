module Core.IntentSpecs (spec) where

import Cardano.Api (lovelaceToValue, serialiseAddress)
import Data.Text qualified as Text (unpack)
import Intent.Example.DSL (
  AddressW (AddressW),
  CanonicalIntent (payTo),
  IntentDSL (PayToW),
  toCanonicalIntent,
  unAdressConwayEra,
 )
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import WBPS.Core.Keys.Ed25519 (PaymentAddess (PaymentAddess), Wallet (Wallet, paymentAddress))
import WBPS.Core.Keys.Ed25519 qualified as Ed25519 (generateWallet)

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
              serialiseAddress (unAdressConwayEra outAddr) `shouldBe` expectedPaymentVerificationKey
            other -> expectationFailure ("unexpected payTo entries: " <> show other)
