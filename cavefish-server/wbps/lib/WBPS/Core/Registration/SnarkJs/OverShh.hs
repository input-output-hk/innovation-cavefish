{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module WBPS.Core.Registration.SnarkJs.OverShh (
  ProvingKeyScheme (..),
  generateProvingKey,
  VerificationKeyScheme (..),
  generateVerificationKey,
) where

import Shh (ExecReference (SearchPath), Proc, load)

load
  SearchPath
  [ "snarkjs" :: String
  ]

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
