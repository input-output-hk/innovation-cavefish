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
import Core.Intent (AddressW (AddressW), IntentDSL (PayToW))
import Core.SP.AskCommitmentProof qualified as AskCommitmentProof
import Core.SP.DemonstrateCommitment qualified as DemonstrateCommitment
import Core.SP.FetchAccount qualified as FetchAccount
import Core.SP.Register qualified as Register
import Data.Default
import Data.Text (pack)
import Network.Wai.Handler.Warp qualified as Warp
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import WBPS.Core.FileScheme (mkFileSchemeFromRoot)
import WBPS.Core.Keys.Ed25519 as Ed25519

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

              userWalletPublicKey <- userWalletPK <$> Ed25519.generateKeyPair

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

              Wallet {publicKey = userWalletPublicKey, paymentAddress = PaymentAddess paymentAddress, ..} <-
                generateWallet

              Register.Outputs {publicVerificationContext, ek} <- register . Register.Inputs $ userWalletPublicKey

              DemonstrateCommitment.Outputs {commitment, txAbs} <-
                demonstrateCommitment
                  . DemonstrateCommitment.Inputs
                    userWalletPublicKey
                  $ PayToW (lovelaceToValue 10_000_000) (AddressW paymentAddress)

              (r, bigR) <- Ed25519.generateKeyTuple

              AskCommitmentProof.Outputs {challenge, proof} <-
                askCommitmentProof
                  . AskCommitmentProof.Inputs userWalletPublicKey
                  $ bigR

              FetchAccount.Outputs {accountMaybe} <- fetchAccount . FetchAccount.Inputs $ userWalletPublicKey

              accountMaybe
                `shouldBe` Just FetchAccount.Account {userWalletPublicKey, ek, publicVerificationContext}
