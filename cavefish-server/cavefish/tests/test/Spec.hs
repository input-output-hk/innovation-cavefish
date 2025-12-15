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
  mkTestCavefishMonad,
 )
import Cardano.Api (lovelaceToValue)
import Cavefish.Endpoints.Read.FetchAccount qualified as FetchAccount
import Cavefish.Endpoints.Write.DemonstrateCommitment qualified as DemonstrateCommitment
import Cavefish.Endpoints.Write.Register qualified as Register
import Control.Monad
import Data.Default (def)
import Intent.Example.DSL (AddressW (AddressW), IntentDSL (PayToW))
import Network.Wai.Handler.Warp qualified as Warp
import Prototype.AskCommitmentProof qualified as AskCommitmentProof
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import WBPS.Core.FileScheme (FileScheme, mkFileSchemeFromRoot)
import WBPS.Core.Keys.Ed25519 (
  PaymentAddess (PaymentAddess),
  UserWalletPublicKey,
  Wallet (Wallet, paymentAddress, publicKey),
  generateKeyPair,
  generateKeyTuple,
  generateWallet,
  userWalletPK,
 )

spec :: Spec
spec = do
  describe "[Cavefish Server - Integration Spec]" $
    describe "Nominal Cases" $ do
      it
        "a user can be registered to a service provider and retrieve (PublicVerificationContext,ek) from the SP"
        $ setupCavefish
          \ServiceProviderAPI {register, fetchAccount} -> do
            userWalletPublicKey <- userWalletPK <$> generateKeyPair

            Register.Outputs {publicVerificationContext, ek} <-
              register . Register.Inputs $ userWalletPublicKey

            FetchAccount.Outputs {accountMaybe} <- fetchAccount . FetchAccount.Inputs $ userWalletPublicKey

            accountMaybe
              `shouldBe` Just FetchAccount.Account {userWalletPublicKey, ek, publicVerificationContext}

-- it
--   "a registered user can request to the SP to demonstrate a commitment for the intent submitted"
--   $ do
--     setupCavefish
--       \ServiceProviderAPI {register, demonstrateCommitment, askCommitmentProof, fetchAccount} -> do
--         (userWalletPublicKey, paymentAddress) <- provisionWallets

--         Register.Outputs {publicVerificationContext, ek} <- register . Register.Inputs $ userWalletPublicKey

--         _ <-
--           demonstrateCommitment
--             . DemonstrateCommitment.Inputs
--               userWalletPublicKey
--             $ PayToW (lovelaceToValue 10_000_000) paymentAddress

--         (_, bigR) <- generateKeyTuple

--         _ <-
--           askCommitmentProof
--             . AskCommitmentProof.Inputs userWalletPublicKey
--             $ bigR

--         FetchAccount.Outputs {accountMaybe} <- fetchAccount . FetchAccount.Inputs $ userWalletPublicKey

--         accountMaybe
--           `shouldBe` Just FetchAccount.Account {userWalletPublicKey, ek, publicVerificationContext}

setupCavefish :: (ServiceProviderAPI -> IO a) -> IO a
setupCavefish action = do
  wbpsScheme <- mkFileSchemeFromRoot "../../wbps"
  Warp.testWithApplication
    (pure $ mkTestCavefishMonad wbpsScheme def)
    (getServiceProviderAPI >=> action)

provisionWallets :: IO (UserWalletPublicKey, AddressW)
provisionWallets = do
  Wallet {publicKey = userWalletPublicKey, paymentAddress = PaymentAddess paymentAddress} <-
    generateWallet
  pure (userWalletPublicKey, AddressW paymentAddress)
