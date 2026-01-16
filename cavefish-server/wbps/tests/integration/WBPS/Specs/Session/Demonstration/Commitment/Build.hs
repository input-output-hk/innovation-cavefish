{-# LANGUAGE QuasiQuotes #-}

module WBPS.Specs.Session.Demonstration.Commitment.Build (specs) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader)
import Data.Default (def)
import Path (reldir)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import WBPS.Adapter.Math.AffinePoint (AffinePoint)
import WBPS.Core.Failure (WBPSFailure)
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (generateKeyPair, userWalletPK)
import WBPS.Core.Session.Demonstration.Artefacts.Commitment (
  CommitmentPayload (unPayload),
  MessageLimbs (unMessageLimbs),
  payload,
 )
import WBPS.Core.Session.Demonstration.Artefacts.Commitment.Build (
  Context (nbCommitmentLimbs),
  Input (Input, ekPowRho, messageBits),
  build,
 )
import WBPS.Core.Session.Demonstration.Artefacts.PreparedMessage.Prepare (toBitsPaddedToMaxSize)
import WBPS.Core.Session.Demonstration.Artefacts.Scalars (
  Scalars (Scalars, ekPowRho),
 )
import WBPS.Core.Session.Demonstration.Artefacts.Scalars.Compute (compute)
import WBPS.Core.Setup.Circuit.FileScheme (FileScheme, defaultFileScheme)
import WBPS.Specs.Adapter.Fixture (
  CommitmentFixtures (CommitmentFixtures, commitmentFixture, messageBitsFixture, unsignedTxFixture),
  commitmentFixtures,
  expectedEkPowRho,
  readFixture,
  sampleEncryptionKey,
  sampleRho,
  whenMismatch,
 )
import WBPS.Specs.Adapter.Test (getRootFolder)
import WBPS.WBPS (runWBPS)

specs :: TestTree
specs =
  testGroup
    "Commitment"
    [ testCase "build emits expected commitment for fixed unsigned tx" $
        commitmentMatchesCircuit
    ]

commitmentMatchesCircuit :: Assertion
commitmentMatchesCircuit = do
  commitmentRootFolders <- getRootFolder [reldir|integration-wpbs-demonstration-build-commitment|]
  let scheme = defaultFileScheme commitmentRootFolders
      fixtures = commitmentFixtures commitmentRootFolders
  runWBPS scheme (runCommitmentFlow fixtures) >>= \case
    Left failures ->
      assertFailure ("Commitment flow failed: " <> show failures)
    Right (commitmentPayload, ekPowRhoActual) -> do
      ekPowRhoActual @?= expectedEkPowRho
      length commitmentPayload @?= nbCommitmentLimbs (def :: Context)
  where
    runCommitmentFlow ::
      (MonadIO m, MonadError [WBPSFailure] m, MonadReader FileScheme m) =>
      CommitmentFixtures ->
      m ([Integer], AffinePoint)
    runCommitmentFlow CommitmentFixtures {unsignedTxFixture, messageBitsFixture, commitmentFixture} = do
      userWalletPublicKey <- liftIO (userWalletPK <$> generateKeyPair)
      Scalars {ekPowRho} <- compute sampleEncryptionKey sampleRho
      message <- liftIO (readFixture unsignedTxFixture)
      messageBitsFromFixture <- liftIO (readFixture messageBitsFixture)
      expectedCommitmentBits <- liftIO (readFixture commitmentFixture)

      whenMismatch "UnsignedTx fixture -> message bits" (toBitsPaddedToMaxSize def message == messageBitsFromFixture)

      commitmentPayload <-
        unPayload . payload
          <$> build userWalletPublicKey Input {ekPowRho, messageBits = messageBitsFromFixture}

      whenMismatch "Commitment payload fixture" (commitmentPayload == expectedCommitmentBits)

      pure (unMessageLimbs commitmentPayload, ekPowRho)
