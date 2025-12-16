{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Spec (spec) where

import Adapter.Cavefish.Client (
  ReadAPI (ReadAPI, fetchAccount),
  ServiceProviderAPI (ServiceProviderAPI, read, write),
  Setup (Setup, alice, bob, serviceProvider),
  WriteAPI (
    WriteAPI,
    askCommitmentProof,
    demonstrateCommitment,
    register
  ),
  setupCavefish,
 )
import Cardano.Api (lovelaceToValue)
import Cavefish.Endpoints.Read.FetchAccount qualified as FetchAccount
import Cavefish.Endpoints.Write.DemonstrateCommitment qualified as DemonstrateCommitment
import Cavefish.Endpoints.Write.Register qualified as Register
import Data.Coerce (coerce)
import Intent.Example.DSL (AddressW (AddressW), IntentDSL (PayToW))
import Prototype.AskCommitmentProof qualified as AskCommitmentProof
import Test.Hspec (Spec, describe, it, shouldBe)
import WBPS.Core.Keys.Ed25519 (
  PaymentAddess (PaymentAddess),
  generateKeyPair,
  generateKeyTuple,
  paymentAddress,
  publicKey,
  userWalletPK,
 )

spec :: Spec
spec = do
  describe "[Cavefish Server - Integration Spec]" $
    describe "Nominal Cases" $ do
      it
        "a user can be registered to a service provider and retrieve (PublicVerificationContext,ek) from the SP"
        $ setupCavefish
          \Setup
             { serviceProvider =
               ServiceProviderAPI
                 { write = WriteAPI {register}
                 , read = ReadAPI {fetchAccount}
                 }
             } -> do
              userWalletPublicKey <- userWalletPK <$> generateKeyPair

              Register.Outputs {publicVerificationContext, ek} <-
                register . Register.Inputs $ userWalletPublicKey

              FetchAccount.Outputs {accountMaybe} <- fetchAccount . FetchAccount.Inputs $ userWalletPublicKey

              accountMaybe
                `shouldBe` Just FetchAccount.Account {userWalletPublicKey, ek, publicVerificationContext}

      it
        "a registered user can request to the SP to demonstrate a commitment for the intent submitted"
        $ do
          setupCavefish
            \Setup
               { serviceProvider =
                 ServiceProviderAPI
                   { write = WriteAPI {register, demonstrateCommitment, askCommitmentProof}
                   , read = ReadAPI {fetchAccount}
                   }
               , alice
               , bob
               } -> do
                Register.Outputs {publicVerificationContext, ek} <- register . Register.Inputs . publicKey $ alice

                DemonstrateCommitment.Outputs {commitment, txAbs} <-
                  demonstrateCommitment
                    . DemonstrateCommitment.Inputs (publicKey alice)
                    $ PayToW (lovelaceToValue 10_000_000) (coerce . paymentAddress $ bob)

                (r, bigR) <- generateKeyTuple

                _ <-
                  askCommitmentProof
                    . AskCommitmentProof.Inputs (publicKey alice)
                    $ bigR

                FetchAccount.Outputs {accountMaybe} <- fetchAccount . FetchAccount.Inputs . publicKey $ alice

                accountMaybe
                  `shouldBe` Just FetchAccount.Account {userWalletPublicKey = publicKey alice, ek, publicVerificationContext}
