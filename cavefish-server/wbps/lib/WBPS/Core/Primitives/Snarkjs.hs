{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module WBPS.Core.Primitives.Snarkjs (
  WitnessScheme (..),
  generateWitness,
  ProveScheme (..),
  generateProof,
  VerifyScheme (..),
  verify,
  ProvingKeyScheme (..),
  generateProvingKey,
  VerificationKeyScheme (..),
  generateVerificationKey,
  exportStatementAsJSON,
) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Shh (ExecReference (SearchPath), Proc, load)

load
  SearchPath
  [ "snarkjs" :: String
  ]

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
  liftIO $
    snarkjs
      ("groth16" :: String)
      ("verify" :: String)
      verificationKey
      statement
      proof

data ProvingKeyScheme = ProvingKeyScheme
  {powerOfTauPrepared :: FilePath, relationR1CS :: FilePath, provingKeyOutput :: FilePath}

generateProvingKey :: ProvingKeyScheme -> Proc ()
generateProvingKey ProvingKeyScheme {..} =
  snarkjs
    ("groth16" :: String)
    ("setup" :: String)
    relationR1CS
    powerOfTauPrepared
    provingKeyOutput

data VerificationKeyScheme = VerificationKeyScheme {provingKey :: FilePath, verificationKeyOutput :: FilePath}

generateVerificationKey :: VerificationKeyScheme -> Proc ()
generateVerificationKey VerificationKeyScheme {..} =
  snarkjs
    ("zkey" :: String)
    ("export" :: String)
    ("verificationkey" :: String)
    provingKey
    verificationKeyOutput

exportStatementAsJSON :: FilePath -> FilePath -> Proc ()
exportStatementAsJSON witness statementOutput =
  snarkjs
    ("wtns" :: String)
    ("export" :: String)
    ("json" :: String)
    witness
    statementOutput
