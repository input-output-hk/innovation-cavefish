{-# LANGUAGE QuasiQuotes #-}

module WBPS.Specs.Adapter.Fixture (
  CommitmentFixtures (..),
  commitmentFixtures,
  readFixture,
  whenMismatch,
  sampleEncryptionKey,
  sampleRho,
  expectedEkPowRho,
) where

import Control.Monad (unless)
import Control.Monad.Except (MonadError, throwError)
import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString.Lazy qualified as BL
import Path (Abs, File, Path, parent, reldir, relfile, toFilePath, (</>))
import WBPS.Adapter.Math.AffinePoint (AffinePoint (AffinePoint))
import WBPS.Core.Failure (WBPSFailure (BuildCommitmentFailed))
import WBPS.Core.Registration.Artefacts.Keys.ElGamal (EncryptionKey (EncryptionKey))
import WBPS.Core.Session.Demonstration.Artefacts.Rho (Rho, mkRho)
import WBPS.Core.Setup.Circuit.FileScheme (RootFolders (RootFolders, input))

data CommitmentFixtures = CommitmentFixtures
  { unsignedTxFixture :: Path Abs File
  , messageBitsFixture :: Path Abs File
  , commitmentFixture :: Path Abs File
  }

commitmentFixtures :: RootFolders -> CommitmentFixtures
commitmentFixtures RootFolders {input} =
  let baseDir = parent input </> [reldir|tests/integration/fixtures/commitment|]
   in CommitmentFixtures
        { unsignedTxFixture = baseDir </> [relfile|unsignedTx.json|]
        , messageBitsFixture = baseDir </> [relfile|messageBits.json|]
        , commitmentFixture = baseDir </> [relfile|commitment.json|]
        }

readFixture :: FromJSON a => Path Abs File -> IO a
readFixture path = do
  bytes <- BL.readFile (toFilePath path)
  case eitherDecode bytes of
    Right value -> pure value
    Left e -> fail ("Failed to decode fixture " <> toFilePath path <> ": " <> e)

whenMismatch ::
  MonadError [WBPSFailure] m =>
  String ->
  Bool ->
  m ()
whenMismatch label ok =
  unless ok $
    throwError [BuildCommitmentFailed (label <> " mismatch in fixture roundtrip")]

sampleEncryptionKey :: EncryptionKey
sampleEncryptionKey =
  EncryptionKey $
    AffinePoint
      13949409190783008520894738635416501547122416709390247001419320903147870232235
      6230067313654301039366684823404445124569608018144478198755770506579514903435

sampleRho :: Rho
sampleRho =
  either (error . ("Invalid rho in test: " <>)) id $
    mkRho 1234567890123456789012345678901234567890

expectedEkPowRho :: AffinePoint
expectedEkPowRho =
  AffinePoint
    9137335444970201643256596907871748556752751480839053976600429028595682443974
    15717005515068243122442038781576298414355055582736444167988846187936743236921
