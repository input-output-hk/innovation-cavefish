{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import WBPS.Specs.Core.Keys.Ed25519 qualified as Ed25519Spec
import WBPS.Specs.Session.Commitment.Scalars qualified as Commitment.Scalars

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "WBPS unit tests"
    [Ed25519Spec.specs, Commitment.Scalars.specs]
