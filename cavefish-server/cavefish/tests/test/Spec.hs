{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Spec (spec) where

import Adapter.Cavefish.Client (
  ServiceProviderAPI (ServiceProviderAPI, fetchAccount, register),
  getServiceProviderAPI,
  mkApplication,
 )
import Core.SP.FetchAccount qualified as FetchAccount
import Core.SP.Register qualified as Register
import Network.Wai.Handler.Warp qualified as Warp
import Test.Hspec (Spec, describe, it, runIO, shouldBe)
import WBPS.Core.FileScheme (mkFileSchemeFromRoot)
import WBPS.Core.Keys.Ed25519 as Ed25519 (
  generateKeyPair,
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
          Warp.testWithApplication (mkApplication wbpsScheme) $
            \port -> do
              ServiceProviderAPI {register, fetchAccount} <- getServiceProviderAPI port

              userWalletPublicKey <- userWalletPK <$> Ed25519.generateKeyPair

              Register.Outputs {publicVerificationContext, ek} <-
                register . Register.Inputs $ userWalletPublicKey

              FetchAccount.Outputs {accountMaybe} <- fetchAccount . FetchAccount.Inputs $ userWalletPublicKey

              accountMaybe
                `shouldBe` Just FetchAccount.Account {userWalletPublicKey, ek, publicVerificationContext}
