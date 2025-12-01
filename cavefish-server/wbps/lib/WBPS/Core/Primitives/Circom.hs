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
  let includeDirClean = dropTrailingPathSeparator includeDir
      includeRoot = takeDirectory includeDirClean
      includeRootRoot = takeDirectory includeRoot
      siblingRoot = takeDirectory includeRootRoot
      topRoot = takeDirectory siblingRoot
      circuitsDirLocal = includeDirClean </> "circuits"
      circuitsDirSibling = includeRoot </> "circuits"
      circuitsBaseSibling = siblingRoot </> "wbps"
      circuitsBaseTop = topRoot </> "wbps"
      vendorCircomlibDir = circuitsBaseSibling </> "vendor" </> "circomlib" </> "circuits"
      vendorCircomlibLocal = includeRoot </> "vendor" </> "circomlib" </> "circuits"
      vendorCircomlibTop = circuitsBaseTop </> "vendor" </> "circomlib" </> "circuits"
      circomlibDirNode = includeRootRoot </> "node_modules" </> "circomlib" </> "circuits"
      poseidon2Dir = circuitsBaseSibling </> "circuits" </> "hashing" </> "poseidon2"
      poseidon2DirTop = circuitsBaseTop </> "circuits" </> "hashing" </> "poseidon2"
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
        vendorCircomlibLocal
        "-l"
        vendorCircomlibDir
        "-l"
        vendorCircomlibTop
        "-l"
        poseidon2Dir
        "-l"
        poseidon2DirTop
        "-l"
        circomlibDirNode

compileBuildCommitmentForFileScheme ::
  MonadReader FileScheme m =>
  m (Proc ())
compileBuildCommitmentForFileScheme =
  asks (compileBuildCommitment . toCompileScheme)

toCompileScheme ::
  FileScheme ->
  BuildCommitmentCompileScheme
toCompileScheme FileScheme {relationCircom} =
  let outputDir = parent relationCircom
   in BuildCommitmentCompileScheme
        { circuitPath = Path.toFilePath relationCircom
        , outputDir = Path.toFilePath outputDir
        , includeDir = Path.toFilePath (parent outputDir)
        }
