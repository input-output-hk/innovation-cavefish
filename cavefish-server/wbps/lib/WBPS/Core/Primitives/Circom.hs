{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module WBPS.Core.Primitives.Circom (
  BuildCommitmentCompileScheme (..),
  compileBuildCommitment,
  compileBuildCommitmentForFileScheme,
) where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Path qualified (parent, toFilePath)
import Shh (ExecReference (SearchPath), Proc, load)
import System.FilePath (dropTrailingPathSeparator, takeDirectory, (</>))
import WBPS.Core.Setup.Circuit.FileScheme qualified as FileScheme
import WBPS.Core.Setup.Circuit.FileScheme qualified as Setup (Setup (buildCommitment))

load
  SearchPath
  [ "circom" :: String
  ]

-- | Fixed set of template arguments required by the BuildCommitment circom template.
data BuildCommitmentCompileScheme = BuildCommitmentCompileScheme
  { circuitPath :: FilePath
  , outputDir :: FilePath
  , includeDir :: FilePath
  }

ensureCircomAvailable :: MonadIO m => m ()
ensureCircomAvailable = do
  missing <- liftIO missingExecutables
  unless (null missing) $
    liftIO . ioError . userError $
      "Missing required executables: " <> unwords missing

compileBuildCommitment :: BuildCommitmentCompileScheme -> Proc ()
compileBuildCommitment BuildCommitmentCompileScheme {..} =
  let includeDirClean = dropTrailingPathSeparator includeDir
      includeRoot = takeDirectory includeDirClean
      includeRootRoot = takeDirectory includeRoot
      siblingRoot = takeDirectory includeRootRoot
      circuitsDirLocal = includeDirClean </> "circuits"
      circuitsDirSibling = includeRoot </> "circuits"
      circuitsBaseSibling = siblingRoot </> "wbps"
      vendorCircomlibDir = circuitsBaseSibling </> "vendor" </> "circomlib" </> "circuits"
      circomlibDirNode = includeRootRoot </> "node_modules" </> "circomlib" </> "circuits"
   in circom
        circuitPath
        ("--r1cs" :: String)
        ("--wasm" :: String)
        ("--sym" :: String)
        ("-o" :: String)
        outputDir
        ("-l" :: String)
        outputDir
        ("-l" :: String)
        includeDir
        ("-l" :: String)
        circuitsDirLocal
        ("-l" :: String)
        circuitsDirSibling
        ("-l" :: String)
        circuitsBaseSibling
        ("-l" :: String)
        vendorCircomlibDir
        ("-l" :: String)
        circomlibDirNode

compileBuildCommitmentForFileScheme ::
  (MonadReader FileScheme.FileScheme m, MonadIO m) =>
  m (Proc ())
compileBuildCommitmentForFileScheme = do
  ensureCircomAvailable
  asks (compileBuildCommitment . toCompileScheme . Setup.buildCommitment . FileScheme.setup)

toCompileScheme ::
  FileScheme.BuildCommitmentSetup ->
  BuildCommitmentCompileScheme
toCompileScheme FileScheme.BuildCommitmentSetup {circom = circomFile} =
  let outputDir = Path.parent circomFile
   in BuildCommitmentCompileScheme
        { circuitPath = Path.toFilePath circomFile
        , outputDir = Path.toFilePath outputDir
        , includeDir = Path.toFilePath (Path.parent outputDir)
        }
