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
import WBPS (getVerificationContext, withFileSchemeIO)
import WBPS.Core (SignerKey)
import WBPS.Core.FileScheme (
  FileScheme (accounts, verificationContext),
  mkFileSchemeFromRoot,
 )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "WBPS unit tests"
    [ testCase "getVerificationContext reuses an existing context file" existingContextReused
    ]

existingContextReused :: IO ()
existingContextReused =
  withSystemTempDirectory "wbps-unit" $ \tmpRoot -> do
    scheme <- mkFileSchemeFromRoot tmpRoot
    let accountsDir = accounts scheme
        verificationRel = verificationContext scheme
    accountDir <- accountDirectoryPath accountsDir testSignerKey
    let verificationPath = Path.toFilePath (accountDir </> verificationRel)
        expectedBytes = BL8.pack "{\"test\":0}"
        expectedValue =
          case Aeson.eitherDecode expectedBytes of
            Left err -> error ("failed to decode expected verification context: " <> err)
            Right value -> value
    createDirectoryIfMissing True (Path.toFilePath accountDir)
    BL.writeFile verificationPath expectedBytes
    result <- withFileSchemeIO scheme (getVerificationContext testSignerKey)
    case result of
      Left err -> assertFailure ("expected existing verification context, but got failure: " <> show err)
      Right value -> assertEqual "verification context value" expectedValue value

accountDirectoryPath :: Path Abs Dir -> SignerKey -> IO (Path Abs Dir)
accountDirectoryPath accountsDir signerKey = do
  relDir <-
    case Path.parseRelDir (show signerKey) of
      Left err -> fail ("invalid account directory: " <> show err)
      Right dir -> pure dir
  pure (accountsDir </> relDir)

testSignerKey :: SignerKey
testSignerKey =
  fromString
    "0ad0ec748542d10fff10d3663dc7ef6a5f82e18a0ec6975fc755e76bae452a8e"
