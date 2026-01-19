{-# LANGUAGE QuasiQuotes #-}

module WBPS.Specs.Session.BlindSigning.Sign (specs) where

import Path (reldir)
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests (QuickCheckTests), testProperty)
import WBPS.Specs.Adapter.BlindSigning (blindSignatureProperty)

specs :: TestTree
specs =
  localOption (QuickCheckTests 1) $
    testGroup
      "BlindSigning"
      [ testProperty
          "blind signature embeds nonce R and satisfies Schnorr equation"
          (blindSignatureProperty [reldir|integration-wbps-sign-flow|])
      ]
