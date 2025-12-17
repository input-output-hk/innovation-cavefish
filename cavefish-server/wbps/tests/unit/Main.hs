{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)
import WBPS.Specs.Core.Keys.Ed25519 qualified as Ed25519Spec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "WBPS unit tests"
    [Ed25519Spec.specs]
