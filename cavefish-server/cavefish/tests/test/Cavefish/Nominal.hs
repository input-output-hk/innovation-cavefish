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
  Setup (Setup, alice, bob, serviceProvider, userToolkit),
  UserToolkitAPI (UserToolkitAPI, assertProofIsValid, signBlindly),
  WriteAPI (
    WriteAPI,
    demonstrate,
    prove,
    register,
    submit
  ),
  setupCavefish,
 )
import Cardano.Api (lovelaceToValue)
import Cavefish.Endpoints.Read.FetchAccount qualified as FetchAccount
import Cavefish.Endpoints.Write.Register qualified as Register
import Cavefish.Endpoints.Write.Session.Demonstrate qualified as Demonstrate
import Cavefish.Endpoints.Write.Session.Prove qualified as Prove
import Cavefish.Endpoints.Write.Session.Submit qualified as Submit
import Data.Coerce (coerce)
import Data.List.NonEmpty qualified as NE
import Intent.Example.DSL (AddressW (AddressW), IntentDSL (AndExpsW, PayToW, SpendFromW), satisfies)
import Path (reldir)
import Test.Hspec (Spec, describe, it, shouldBe)
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (
  PaymentAddess (PaymentAddess),
  keyPair,
  paymentAddress,
  publicKey,
 )
import WBPS.Core.Session.Steps.BlindSigning.ThetaStatement (rebuildThetaStatement)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Commitment (Commitment (Commitment, payload))
import WBPS.Core.Session.Steps.Demonstration.Artefacts.PreparedMessage (PublicMessage (PublicMessage))
import WBPS.Core.Session.Steps.Demonstration.Artefacts.R qualified as R

spec :: Spec
spec = do
  describe "[Cavefish Server - Integration Spec]" $
    describe "Nominal Cases" $ do
      it
        "register, demonstrate, prove, verify, blindly-sign and end up with signed transaction"
        $ do
          setupCavefish
            [reldir|integration-cavefish-nominal-flow|]
            \Setup
               { serviceProvider =
                 ServiceProviderAPI
                   { write = WriteAPI {register, demonstrate, prove, submit}
                   , read = ReadAPI {fetchAccount}
                   }
               , userToolkit = UserToolkitAPI {assertProofIsValid, signBlindly}
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
                Register.Outputs {registrationId, publicVerificationContext, ek} <- register . Register.Inputs . publicKey $ alice

                Demonstrate.Outputs {sessionId, commitment = commitment@Commitment {payload}, txAbs} <-
                  demonstrate
                    . Demonstrate.Inputs registrationId
                    $ intent

                satisfies intent txAbs `shouldBe` True

                (r, bigR) <- R.generateKeyTuple

                Prove.Outputs {challenge, proof} <-
                  prove
                    Prove.Inputs
                      { sessionId
                      , bigR = bigR
                      }

                assertProofIsValid
                  publicVerificationContext
                  (rebuildThetaStatement (publicKey alice) bigR challenge payload (PublicMessage txAbs))
                  proof

                signature <- signBlindly (keyPair alice) r challenge

                Submit.Outputs {txId} <-
                  submit
                    Submit.Inputs
                      { sessionId
                      , signature
                      }

                FetchAccount.Outputs {accountMaybe} <- fetchAccount . FetchAccount.Inputs $ registrationId

                accountMaybe
                  `shouldBe` Just FetchAccount.Account {registrationId, ek, publicVerificationContext}
