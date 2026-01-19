{-# LANGUAGE QuasiQuotes #-}

module WBPS.Core.Session.BlindSigning.VerifyProof (
  assertProofIsValid,
) where

import Control.Monad (unless)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Char (isAlphaNum, isSpace, toLower)
import Path (relfile, toFilePath, (</>))
import Path.IO (withSystemTempDir)
import Shh (Stream (StdOut), captureTrim, (&!>), (|>))
import WBPS.Adapter.Path (writeTo)
import WBPS.Core.Failure (WBPSFailure (ProofVerificationFailed))
import WBPS.Core.Primitives.Snarkjs qualified as Snarkjs
import WBPS.Core.Registration.Artefacts.Groth16.Setup (
  PublicVerificationContextAsJSON,
 )
import WBPS.Core.Session.BlindSigning.ThetaStatement (ThetaStatement)
import WBPS.Core.Session.Proving.Artefacts.Proof (Proof)

assertProofIsValid ::
  (MonadIO m, MonadError [WBPSFailure] m) =>
  PublicVerificationContextAsJSON ->
  ThetaStatement ->
  Proof ->
  m Proof
assertProofIsValid verificationContextAsJSON statement proof = do
  output <-
    liftIO $
      withSystemTempDir "wbps-verify-proof-" $ \tmpDir -> do
        let verificationKeyPath = tmpDir </> [relfile|verification_key.json|]
            statementPath = tmpDir </> [relfile|statement.json|]
            proofPath = tmpDir </> [relfile|proof.json|]
        writeTo verificationKeyPath verificationContextAsJSON
        writeTo statementPath statement
        writeTo proofPath proof
        Snarkjs.verify
          Snarkjs.VerifyScheme
            { verificationKey = toFilePath verificationKeyPath
            , statement = toFilePath statementPath
            , proof = toFilePath proofPath
            }
          &!> StdOut
          |> captureTrim
  unless (isVerificationOk output) $
    throwError [ProofVerificationFailed (verificationFailedMessage output)]
  pure proof

isVerificationOk :: BL8.ByteString -> Bool
isVerificationOk output =
  let tokens = normalizeOutputTokens output
      hasInvalid =
        "invalid" `elem` tokens
          || "fail" `elem` tokens
          || "failed" `elem` tokens
          || ("not" `elem` tokens && "valid" `elem` tokens)
      hasValid = any (`elem` tokens) ["ok", "true", "valid"]
   in hasValid && not hasInvalid

verificationFailedMessage :: BL8.ByteString -> String
verificationFailedMessage output =
  let rawOutput = stripOutput (BL8.unpack output)
   in if null rawOutput
        then "Proof verification failed: snarkjs produced no output (stdout/stderr empty)."
        else "Proof verification failed (snarkjs output): " <> rawOutput

normalizeOutputTokens :: BL8.ByteString -> [String]
normalizeOutputTokens output =
  let normalized = map normalizeChar (BL8.unpack output)
   in filter (not . null) (words normalized)
  where
    normalizeChar c
      | isAlphaNum c = toLower c
      | otherwise = ' '

stripOutput :: String -> String
stripOutput = dropWhile isSpace . reverse . dropWhile isSpace . reverse
