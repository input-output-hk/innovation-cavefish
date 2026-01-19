module WBPS.Specs.Adapter.FixtureIO (
  readJson,
  assertFileExists,
  findRepoRoot,
) where

import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString.Lazy qualified as BL
import System.Directory (doesFileExist, getCurrentDirectory)
import System.FilePath (takeDirectory, (</>))
import Test.Tasty.HUnit (assertFailure)

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
    else assertFailure ("Missing fixture: " <> path)

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
