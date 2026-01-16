module WBPS.Specs.Adapter.Test (getRootFolder) where

import Path (Abs, Dir, Path, Rel, parseAbsDir, (</>))
import Path.IO (ensureDir, listDir, removeDirRecur, removeFile)
import System.Environment (getEnv)
import WBPS.Core.Setup.Circuit.FileScheme (RootFolders (RootFolders, input, output))

getRootFolder :: Path Rel Dir -> IO RootFolders
getRootFolder dirLabel = do
  inputRoot <- requiredEnvDir "WBPS_TEST_INPUT_ROOT"
  outputRoot <- requiredEnvDir "WBPS_TEST_OUTPUT_ROOT"
  ensureDir $ outputRoot </> dirLabel
  clearDir $ outputRoot </> dirLabel

  pure
    RootFolders
      { input = inputRoot
      , output = outputRoot </> dirLabel
      }

requiredEnvDir :: String -> IO (Path Abs Dir)
requiredEnvDir envVar = parseAbsDir =<< getEnv envVar

clearDir :: Path Abs Dir -> IO ()
clearDir dir = do
  (dirs, files) <- listDir dir
  mapM_ removeDirRecur dirs
  mapM_ removeFile files
