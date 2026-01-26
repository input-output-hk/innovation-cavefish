{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module WBPS.Adapter.CLI.Wrapped.Snarkjs (
  ensureSnarkjsAvailable,
  WitnessScheme (..),
  generateWitness,
  ProveScheme (..),
  generateProof,
  VerifyScheme (..),
  verify,
  exportStatementAsJSON,
) where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Shh (ExecReference (SearchPath), Proc, load)

{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}

load
  SearchPath
  [ "snarkjs" :: String
  ]

ensureSnarkjsAvailable :: MonadIO m => m ()
ensureSnarkjsAvailable = do
  missing <- liftIO missingExecutables
  unless (null missing) $
    liftIO . ioError . userError $
      "Missing required executables: " <> unwords missing

data WitnessScheme = WitnessScheme {wasm :: FilePath, input :: FilePath, witnessOutput :: FilePath}

generateWitness :: WitnessScheme -> Proc ()
generateWitness WitnessScheme {..} =
  snarkjs
    ("wtns" :: String)
    ("calculate" :: String)
    wasm
    input
    witnessOutput

data ProveScheme = ProveScheme
  { provingKey :: FilePath
  , witness :: FilePath
  , proofOutput :: FilePath
  , statementOutput :: FilePath
  }

generateProof :: ProveScheme -> Proc ()
generateProof ProveScheme {..} =
  snarkjs
    ("groth16" :: String)
    ("prove" :: String)
    provingKey
    witness
    proofOutput
    statementOutput

data VerifyScheme = VerifyScheme {verificationKey :: FilePath, statement :: FilePath, proof :: FilePath}

verify :: VerifyScheme -> Proc ()
verify VerifyScheme {..} =
  snarkjs
    ("groth16" :: String)
    ("verify" :: String)
    verificationKey
    statement
    proof

exportStatementAsJSON :: FilePath -> FilePath -> Proc ()
exportStatementAsJSON witness statementOutput =
  snarkjs
    ("wtns" :: String)
    ("export" :: String)
    ("json" :: String)
    witness
    statementOutput
