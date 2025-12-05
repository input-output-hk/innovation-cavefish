{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module WBPS.Core.Primitives.Circom (
  BuildCommitmentParams (..),
  BuildCommitmentCompileScheme (..),
  compileBuildCommitment,
  compileBuildCommitmentForFileScheme,
) where

import Control.Monad.Reader (MonadReader, asks)
import Path (parent)
import Path qualified
import Shh
import System.FilePath (dropTrailingPathSeparator, takeDirectory, (</>))
import WBPS.Core.FileScheme

load
  SearchPath
  [ "echo"
  , "circom"
  ]

-- | Fixed set of template arguments required by the BuildCommitment circom template.
data BuildCommitmentParams = BuildCommitmentParams
  { messageSize :: Int
  , commitmentLimbSize :: Int
  , nbCommitmentLimbs :: Int
  }

data BuildCommitmentCompileScheme = BuildCommitmentCompileScheme
  { circuitPath :: FilePath
  , outputDir :: FilePath
  , includeDir :: FilePath
  }

compileBuildCommitment :: BuildCommitmentCompileScheme -> Proc ()
compileBuildCommitment BuildCommitmentCompileScheme {..} =
  compileCircom circuitPath outputDir includeDir

compileBuildCommitmentForFileScheme ::
  MonadReader FileScheme m =>
  m (Proc ())
compileBuildCommitmentForFileScheme =
  asks (compileBuildCommitment . toCompileScheme)

toCompileScheme ::
  FileScheme ->
  BuildCommitmentCompileScheme
toCompileScheme =
  toCompileSchemeWith BuildCommitmentCompileScheme

compileCircom :: FilePath -> FilePath -> FilePath -> Proc ()
compileCircom circuitPath outputDir includeDir =
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
        "--r1cs"
        "--wasm"
        "--sym"
        "-o"
        outputDir
        "-l"
        outputDir
        "-l"
        includeDir
        "-l"
        circuitsDirLocal
        "-l"
        circuitsDirSibling
        "-l"
        circuitsBaseSibling
        "-l"
        vendorCircomlibDir
        "-l"
        circomlibDirNode

toCompileSchemeWith ::
  (FilePath -> FilePath -> FilePath -> a) ->
  FileScheme ->
  a
toCompileSchemeWith mkScheme FileScheme {relationCircom} =
  let outputDir = parent relationCircom
   in mkScheme
        (Path.toFilePath relationCircom)
        (Path.toFilePath outputDir)
        (Path.toFilePath (parent outputDir))
