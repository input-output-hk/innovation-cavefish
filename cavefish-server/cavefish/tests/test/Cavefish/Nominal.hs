{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Cavefish.Nominal (spec) where

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
import Cavefish.Endpoints.Write.AskCommitmentProof qualified as AskCommitmentProof
import Cavefish.Endpoints.Write.DemonstrateCommitment qualified as DemonstrateCommitment
import Cavefish.Endpoints.Write.Register qualified as Register
import Data.Coerce (coerce)
import Data.List.NonEmpty qualified as NE
import Intent.Example.DSL (AddressW (AddressW), IntentDSL (AndExpsW, PayToW, SpendFromW), satisfies)
import Path (reldir)
import Test.Hspec (Spec, describe, it, shouldBe)
import WBPS.Core.Keys.Ed25519 (
  PaymentAddess (PaymentAddess),
  generateKeyPair,
  generateKeyTuple,
  paymentAddress,
  publicKey,
  userWalletPK,
 )
import WBPS.Core.Session.Commitment (Commitment (Commitment, id, payload))
import WBPS.Core.Session.R (R (R))
import Prelude hiding (id)

spec :: Spec
spec = do
  describe "[Cavefish Server - Integration Spec]" $
    describe "Nominal Cases" $ do
      it
        "a user can be registered to a service provider and retrieve (PublicVerificationContext,ek) from the SP"
        $ setupCavefish
          [reldir|Cavefish-Spec-Register|]
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
            [reldir|Cavefish-Spec-RegisterAndDemonstrateCommitment|]
            \Setup
               { serviceProvider =
                 ServiceProviderAPI
                   { write = WriteAPI {register, demonstrateCommitment, askCommitmentProof}
                   , read = ReadAPI {fetchAccount}
                   }
               , alice
               , bob
               } -> do
                let intent =
                      AndExpsW
                        ( NE.fromList
                            [ SpendFromW (coerce . paymentAddress $ alice)
                            , PayToW (lovelaceToValue 10_000_000) (coerce . paymentAddress $ bob)
                            ]
                        )
                Register.Outputs {publicVerificationContext, ek} <- register . Register.Inputs . publicKey $ alice

                DemonstrateCommitment.Outputs {commitment = Commitment {id, payload}, txAbs} <-
                  demonstrateCommitment
                    . DemonstrateCommitment.Inputs (publicKey alice)
                    $ intent

                satisfies intent txAbs `shouldBe` True

                (r, bigR) <- generateKeyTuple

                _ <-
                  askCommitmentProof
                    AskCommitmentProof.Inputs
                      { userWalletPublicKey = publicKey alice
                      , commitmentId = id
                      , bigR = R bigR
                      }

                FetchAccount.Outputs {accountMaybe} <- fetchAccount . FetchAccount.Inputs . publicKey $ alice

                accountMaybe
                  `shouldBe` Just FetchAccount.Account {userWalletPublicKey = publicKey alice, ek, publicVerificationContext}
