{-# LANGUAGE QuasiQuotes #-}

module WBPS.Specs.Session.Submitting.Submit (specs) where

import Control.Monad.Except (catchError)
import Data.Bits (xor)
import Data.Word (Word8)
import Path (reldir)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase)
import WBPS.Core.Failure (WBPSFailure (SessionSubmittingFailed))
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (generateKeyPair, userWalletPK)
import WBPS.Core.Registration.Register (register)
import WBPS.Core.Registration.RegistrationId (RegistrationId (RegistrationId))
import WBPS.Core.Session.Steps.BlindSigning.BlindSignature (BlindSignature (BlindSignature), sign)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.R (generateKeyTuple)
import WBPS.Core.Session.Steps.Demonstration.Demonstrate (demonstrate)
import WBPS.Core.Session.Steps.Proving.Persistence.Events qualified as Proved
import WBPS.Core.Session.Steps.Proving.Prove (prove)
import WBPS.Core.Session.Steps.Proving.Proved (CommitmentProved (CommitmentProved, challenge))
import WBPS.Core.Session.Steps.Submitting.Submit (submit)
import WBPS.Core.Setup.Circuit.FileScheme (defaultFileScheme)
import WBPS.Specs.Adapter.Fixture (CommitmentFixtures (unsignedTxFixture), commitmentFixtures, readFixture)
import WBPS.Specs.Adapter.Test (getRootFolder)
import WBPS.WBPS (runWBPS)

specs :: TestTree
specs =
  testGroup
    "Submitting"
    [ testCase "submit validates blind signature" submitValidatesSignature
    ]

submitValidatesSignature :: Assertion
submitValidatesSignature = do
  rootFolders <- getRootFolder [reldir|integration-wbps-submit-flow|]
  let fileScheme = defaultFileScheme rootFolders
  unsignedTx <- readFixture . unsignedTxFixture . commitmentFixtures $ rootFolders
  keyPair <- generateKeyPair
  (nonceSecret, noncePublic) <- generateKeyTuple

  result <-
    runWBPS fileScheme $ do
      let userWalletPublicKey = userWalletPK keyPair
      let registrationId = RegistrationId userWalletPublicKey
      _ <- register userWalletPublicKey
      (sessionId, _) <- demonstrate registrationId unsignedTx
      Proved.EventHistory {proved = CommitmentProved {challenge}} <- prove sessionId noncePublic
      signature <- sign keyPair nonceSecret challenge
      _ <- submit (const (pure ())) sessionId signature
      let badSignature = tamperSignature signature
      (submit (const (pure ())) sessionId badSignature >> pure Nothing)
        `catchError` (pure . Just)

  case result of
    Left failures ->
      assertFailure ("submit flow failed: " <> show failures)
    Right Nothing ->
      assertFailure "expected submit to reject a tampered signature"
    Right (Just failures) ->
      assertBool "expected SessionSubmittingFailed" (any isSessionSubmittingFailed failures)

tamperSignature :: BlindSignature -> BlindSignature
tamperSignature (BlindSignature bytes) =
  case bytes of
    [] -> BlindSignature [1]
    (b : rest) -> BlindSignature (flipBit b : rest)
  where
    flipBit :: Word8 -> Word8
    flipBit byte = byte `xor` 1

isSessionSubmittingFailed :: WBPSFailure -> Bool
isSessionSubmittingFailed failure =
  case failure of
    SessionSubmittingFailed _ -> True
    _ -> False
