{-# LANGUAGE OverloadedStrings #-}

module Core.IntentDSLGen (
  genDSL,
) where

import Cardano.Api qualified as Api
import Cardano.Api.Era (ShelleyBasedEra (ShelleyBasedEraConway))
import Control.Monad.IO.Class (liftIO)
import Cooked qualified
import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty ((:|)), singleton)
import Data.Text (pack)
import Debug.Trace qualified as Debug
import Hedgehog (Gen)
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Intent.Example.DSL (
  AddressW (AddressW),
  AdressConwayEra (AdressConwayEra),
  IntentDSL (AndExpsW, ChangeToW, MaxFeeW, MaxIntervalW, MustMintW, PayToW, SpendFromW),
 )
import Ledger.CardanoWallet qualified
import Ledger.CardanoWallet qualified as Ledger
import System.IO.Unsafe (unsafePerformIO)
import Test.Gen.Cardano.Api.Typed (
  genAssetId,
  genQuantity,
  genValue,
 )
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (
  KeyPair,
  PaymentAddess (..),
  Wallet (Wallet, paymentAddress),
  generateWallet,
 )

genWallet :: Gen Wallet
genWallet = pure $ unsafePerformIO $ generateWallet

addressWFromWallet :: Wallet -> AddressW
addressWFromWallet = AddressW . unPaymentAddess . paymentAddress

genPayToW :: AddressW -> Gen IntentDSL
genPayToW addressW = do
  value <- genValue (pure $ Api.AdaAssetId) (genQuantity $ Range.constant 13 17)
  pure $ PayToW value addressW

genDSL :: Gen IntentDSL
genDSL = do
  {- TODO Minting is not supported yet. Jan/22/26, -KK
    mustMintW <- MustMintW <$> genValue genAssetId (genQuantity $ Range.linear 1 10)
  -}
  let
    alice = AddressW . Api.serialiseAddress $ Ledger.CardanoWallet.mockWalletAddress $ Cooked.wallet 1
    mary = AddressW . Api.serialiseAddress $ Ledger.CardanoWallet.mockWalletAddress $ Cooked.wallet 2
    bob = AddressW . Api.serialiseAddress $ Ledger.CardanoWallet.mockWalletAddress $ Cooked.wallet 3
    spendFromW = SpendFromW alice
    changeToW = ChangeToW mary
  payToW <- genPayToW bob
  maxFeeW <- MaxFeeW <$> Gen.integral (Range.constant 10 15)
  pure $ AndExpsW $ spendFromW :| [payToW, changeToW, maxFeeW]
