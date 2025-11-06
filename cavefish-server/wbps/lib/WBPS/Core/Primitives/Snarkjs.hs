{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module WBPS.Core.Primitives.Snarkjs 
    ( WitnessScheme(..)
    , generateWitness
    , ProveScheme(..)
    , generateProof
    , VerifyScheme(..)
    , verify
    , ProvingKeyScheme(..)
    , generateProvingKey
    , VerificationKeyScheme(..)
    , generateVerificationKey
    , exportStatementAsJSON
    ) where

import Shh
import Control.Monad.IO.Class


load SearchPath
    [ "echo"
    , "snarkjs"
    ]

data WitnessScheme =  WitnessScheme {wasm :: FilePath, input :: FilePath , witnessOutput :: FilePath} 
generateWitness :: WitnessScheme -> Proc ()
generateWitness WitnessScheme {..} 
    = snarkjs
        "wtns"
        "calculate"
        wasm
        input
        witnessOutput
        

data ProveScheme = ProveScheme { provingKey :: FilePath, witness :: FilePath, proofOutput :: FilePath , statementOutput :: FilePath} 

generateProof :: ProveScheme -> Proc()
generateProof ProveScheme {..} 
    = snarkjs
        "groth16"
        "prove"
        provingKey
        witness 
        proofOutput
        statementOutput

data VerifyScheme = VerifyScheme {verificationKey:: FilePath, statement :: FilePath , proof :: FilePath} 
verify ::  VerifyScheme -> Proc ()
verify VerifyScheme {..} 
    = liftIO $ snarkjs
        "groth16"
        "verify"
        verificationKey
        statement
        proof


data ProvingKeyScheme =  ProvingKeyScheme {powerOfTauPrepared :: FilePath, relationR1CS :: FilePath, provingKeyOutput :: FilePath}

generateProvingKey :: ProvingKeyScheme -> Proc ()
generateProvingKey ProvingKeyScheme {..}
    = snarkjs
        "groth16"
        "setup"
        relationR1CS
        powerOfTauPrepared
        provingKeyOutput

data VerificationKeyScheme = VerificationKeyScheme { provingKey :: FilePath, verificationKeyOutput :: FilePath } 

generateVerificationKey :: VerificationKeyScheme -> Proc ()
generateVerificationKey VerificationKeyScheme {..} 
    = snarkjs
        "zkey"
        "export"
        "verificationkey"
        provingKey
        verificationKeyOutput

exportStatementAsJSON :: FilePath -> FilePath -> Proc ()
exportStatementAsJSON witness statementOutput 
    = snarkjs 
        "wtns" 
        "export" 
        "json" 
        witness 
        statementOutput
