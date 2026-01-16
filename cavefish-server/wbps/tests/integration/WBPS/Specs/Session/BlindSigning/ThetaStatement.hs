module WBPS.Specs.Session.BlindSigning.ThetaStatement (specs) where

import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString.Lazy qualified as BL
import Data.String (fromString)
import Data.Text (Text)
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath (takeDirectory, (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Session.BlindSigning.ThetaStatement (
  ThetaStatement (ThetaStatement),
  rebuildThetaStatementFromDemonstrated,
 )
import WBPS.Core.Session.Demonstration.Artefacts.Commitment (Commitment)
import WBPS.Core.Session.Demonstration.Artefacts.PreparedMessage (PreparedMessage)
import WBPS.Core.Session.Demonstration.Artefacts.R (R)
import WBPS.Core.Session.Demonstration.Artefacts.Scalars (Scalars)
import WBPS.Core.Session.Demonstration.Demonstrated (
  CommitmentDemonstrated (CommitmentDemonstrated),
 )
import WBPS.Core.Session.Proving.Artefacts.Challenge (Challenge)

specs :: TestTree
specs =
  testGroup
    "BlindSigning"
    [ testCase "rebuilds statement.json from output artefacts" rebuildsStatement
    ]

rebuildsStatement :: Assertion
rebuildsStatement = do
  repoRoot <- findRepoRoot
  let statementPath = fixtureRoot repoRoot </> "proved/statement.json"
      challengePath = fixtureRoot repoRoot </> "proved/challenge.json"
      bigRPath = fixtureRoot repoRoot </> "proved/big_r.json"
      commitmentPath = fixtureRoot repoRoot </> "demonstrated/commitment.json"
      preparedMessagePath = fixtureRoot repoRoot </> "demonstrated/preparedMessage.json"
      scalarsPath = fixtureRoot repoRoot </> "demonstrated/scalars.json"
      requiredFiles =
        [ statementPath
        , challengePath
        , bigRPath
        , commitmentPath
        , preparedMessagePath
        , scalarsPath
        ]

  mapM_ assertFileExists requiredFiles
  expected <- readJson statementPath :: IO [Text]
  commitment <- readJson commitmentPath :: IO Commitment
  preparedMessage <- readJson preparedMessagePath :: IO PreparedMessage
  scalars <- readJson scalarsPath :: IO Scalars
  challenge <- readJson challengePath :: IO Challenge
  bigR <- readJson bigRPath :: IO R

  let userWalletPublicKey = fromString accountIdHex :: UserWalletPublicKey
      commitmentDemonstrated = CommitmentDemonstrated preparedMessage scalars commitment
      ThetaStatement actual =
        rebuildThetaStatementFromDemonstrated userWalletPublicKey bigR challenge commitmentDemonstrated

  actual @?= expected

readJson :: FromJSON a => FilePath -> IO a
readJson path = do
  bytes <- BL.readFile path
  case eitherDecode bytes of
    Right value -> pure value
    Left err -> fail ("Failed to decode " <> path <> ": " <> err)

assertFileExists :: FilePath -> IO ()
assertFileExists path = do
  exists <- doesFileExist path
  if exists
    then pure ()
    else assertFailure ("Missing output fixture: " <> path)

accountIdHex :: String
accountIdHex =
  "e62857e45795b0e3637ca3f525f94fd5d7c7025941016b960d4e648a2b1e6ad4"

sessionIdHex :: String
sessionIdHex =
  "e2b78707b70dc324b0ca7828e084668a717fdcc610f2086326980b27829aa941"

fixtureRoot :: FilePath -> FilePath
fixtureRoot repoRoot =
  repoRoot
    </> "output/tests/integration-cavefish-nominal-flow/accounts"
    </> accountIdHex
    </> "sessions"
    </> sessionIdHex

findRepoRoot :: IO FilePath
findRepoRoot = do
  cwd <- getCurrentDirectory
  go cwd
  where
    go dir = do
      let marker = dir </> "cabal.project"
      exists <- doesFileExist marker
      if exists
        then pure dir
        else do
          let parent = takeDirectory dir
          if parent == dir
            then fail ("Could not locate cabal.project from " <> dir)
            else go parent
