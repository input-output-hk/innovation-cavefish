{-# LANGUAGE QuasiQuotes #-}

module WBPS.Specs.Session.Commitment.Build (specs) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader)
import Data.Default (def)
import Path (reldir)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import WBPS.Core.Failure (RegistrationFailed)
import WBPS.Core.FileScheme (FileScheme, defaultFileScheme)
import WBPS.Core.Keys.ElGamal qualified as ElGamal
import WBPS.Core.Session.Commitment (CommitmentPayload (unPayload), payload)
import WBPS.Core.Session.Commitment.Build (
  Context (nbCommitmentLimbs),
  Input (Input, ekPowRho, messageBits),
  build,
 )
import WBPS.Core.Session.Commitment.Scalars (
  CommitmentScalars (CommitmentScalars, ekPowRho),
 )
import WBPS.Core.Session.Commitment.Scalars.Compute (compute)
import WBPS.Core.ZK.Message (MessageBits (unMessageBits), messageToBits)
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
  commitmentRootFolders <- getRootFolder [reldir|WBPS-integration-specs-commitmentMatchesCircuit|]
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
      (MonadIO m, MonadError [RegistrationFailed] m, MonadReader FileScheme m) =>
      CommitmentFixtures ->
      m ([Integer], ElGamal.AffinePoint)
    runCommitmentFlow CommitmentFixtures {unsignedTxFixture, messageBitsFixture, commitmentFixture} = do
      CommitmentScalars {ekPowRho} <- compute sampleEncryptionKey sampleRho
      message <- liftIO (readFixture unsignedTxFixture)
      messageBitsFromFixture <- liftIO (readFixture messageBitsFixture)
      expectedCommitmentBits <- liftIO (readFixture commitmentFixture)

      whenMismatch "UnsignedTx fixture -> message bits" (messageToBits def message == messageBitsFromFixture)

      commitmentPayload <- unPayload . payload <$> build Input {ekPowRho, messageBits = messageBitsFromFixture}

      whenMismatch "Commitment payload fixture" (commitmentPayload == expectedCommitmentBits)

      pure (unMessageBits commitmentPayload, ekPowRho)
