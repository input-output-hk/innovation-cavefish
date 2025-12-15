{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Spec (spec) where

import Adapter.Cavefish.Client (
  ServiceProviderAPI (
    ServiceProviderAPI,
    askCommitmentProof,
    demonstrateCommitment,
    fetchAccount,
    register
  ),
  getServiceProviderAPI,
  mkApplication,
 )
import Cardano.Api (lovelaceToValue)
import Cavefish.Endpoints.Read.FetchAccount qualified as FetchAccount
import Cavefish.Endpoints.Write.DemonstrateCommitment qualified as DemonstrateCommitment
import Cavefish.Endpoints.Write.Register qualified as Register
import Data.Default (def)
import Intent.Example.DSL (AddressW (AddressW), IntentDSL (PayToW))
import Network.Wai.Handler.Warp qualified as Warp
import Prototype.AskCommitmentProof qualified as AskCommitmentProof
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import WBPS.Core.FileScheme (mkFileSchemeFromRoot)
import WBPS.Core.Keys.Ed25519 (
  PaymentAddess (PaymentAddess),
  Wallet (Wallet, paymentAddress, publicKey),
  generateKeyPair,
  generateKeyTuple,
  generateWallet,
  userWalletPK,
 )

spec :: Spec
spec = do
  wbpsScheme <- runIO (mkFileSchemeFromRoot "../../wbps")
  describe "[Cavefish Server - Integration Spec]" $
    describe "Nominal Cases" $ do
      it
        "a user can be registered to a service provider and retrieve (PublicVerificationContext,ek) from the SP"
        $ do
          Warp.testWithApplication (pure $ mkApplication wbpsScheme def) $
            \port -> do
              ServiceProviderAPI {register, fetchAccount} <- getServiceProviderAPI port

              userWalletPublicKey <- userWalletPK <$> generateKeyPair

              Register.Outputs {publicVerificationContext, ek} <-
                register . Register.Inputs $ userWalletPublicKey

              FetchAccount.Outputs {accountMaybe} <- fetchAccount . FetchAccount.Inputs $ userWalletPublicKey

              accountMaybe
                `shouldBe` Just FetchAccount.Account {userWalletPublicKey, ek, publicVerificationContext}

      it
        "a registered user can request to the SP to demonstrate a commitment for the intent submitted"
        $ do
          Warp.testWithApplication (pure $ mkApplication wbpsScheme def) $
            \port -> do
              ServiceProviderAPI {register, demonstrateCommitment, askCommitmentProof, fetchAccount} <-
                getServiceProviderAPI port

              Wallet {publicKey = userWalletPublicKey, paymentAddress = PaymentAddess paymentAddress} <-
                generateWallet

              Register.Outputs {publicVerificationContext, ek} <- register . Register.Inputs $ userWalletPublicKey

              _ <-
                demonstrateCommitment
                  . DemonstrateCommitment.Inputs
                    userWalletPublicKey
                  $ PayToW (lovelaceToValue 10_000_000) (AddressW paymentAddress)

              (_, bigR) <- generateKeyTuple

              _ <-
                askCommitmentProof
                  . AskCommitmentProof.Inputs userWalletPublicKey
                  $ bigR

              FetchAccount.Outputs {accountMaybe} <- fetchAccount . FetchAccount.Inputs $ userWalletPublicKey

              accountMaybe
                `shouldBe` Just FetchAccount.Account {userWalletPublicKey, ek, publicVerificationContext}
