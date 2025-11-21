{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.String (fromString)
import Path (Abs, Dir, Path, (</>))
import Path qualified
import System.Directory (createDirectoryIfMissing)
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import WBPS (withFileSchemeIO)
import WBPS.Core.FileScheme (
  FileScheme (accounts, verificationContext),
  mkFileSchemeFromRoot,
 )
import WBPS.Core.Keys.Ed25519 (UserWalletPublicKey)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "WBPS unit tests"
    []
