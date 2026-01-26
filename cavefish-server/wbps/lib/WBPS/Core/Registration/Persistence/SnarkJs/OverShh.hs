{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module WBPS.Core.Registration.Persistence.SnarkJs.OverShh (
  ensureSnarkjsAvailable,
  ProvingKeyScheme (..),
  generateProvingKey,
  VerificationKeyScheme (..),
  generateVerificationKey,
) where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
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
