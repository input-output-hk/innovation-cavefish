{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module WBPS.Core.Session.Demonstration.Artefacts.Commitment.Build (
  build,
  Context (..),
  Input (..),
) where

import Control.Monad (unless)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask, asks)
import Crypto.Random (getRandomBytes)
import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as AesonTypes
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Default (Default (def))
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V
import Path (Abs, Dir, File, Path, Rel, reldir, toFilePath, (</>))
import Path.IO (createTempDir, doesFileExist, ensureDir, renameDir)
import Shh (Proc, Stream (Append, StdOut), (&!>), (&>))
import System.FilePath qualified as FP
import Text.Hex (encodeHex)
import Text.Read (readMaybe)
import WBPS.Adapter.Math.AffinePoint (AffinePoint (AffinePoint), x, y)
import WBPS.Adapter.Monad.Control (allM)
import WBPS.Adapter.Path (writeTo)
import WBPS.Core.Failure (
  WBPSFailure,
  toWBPSFailure,
 )
import WBPS.Core.Primitives.Circom (
  compileBuildCommitmentForFileScheme,
 )
import WBPS.Core.Primitives.Snarkjs qualified as Snarkjs
import WBPS.Core.Primitives.SnarkjsOverFileScheme (getGenerateBuildCommitmentWitnessProcess)
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Registration.FileScheme (deriveAccountDirectoryFrom)
import WBPS.Core.Session.Demonstration.Artefacts.Commitment (
  Commitment (Commitment, id),
  CommitmentPayload (CommitmentPayload),
  MessageLimbs (MessageLimbs),
  mkCommitment,
 )
import WBPS.Core.Session.Demonstration.Artefacts.PreparedMessage (MessageBits)
import WBPS.Core.Session.FileScheme (deriveSessionDirectoryFrom)
import WBPS.Core.Setup.Circuit.FileScheme (
  Account (session, shellLogs),
  BuildCommitmentInternals (input, output, statementOutput),
  BuildCommitmentSetup (BuildCommitmentSetup, r1cs, wasm),
  FileScheme (account, setup),
  buildCommitmentInternals,
  demonstration,
 )
import WBPS.Core.Setup.Circuit.FileScheme qualified as Filescheme
import WBPS.Core.Setup.Circuit.FileScheme qualified as Setup (Setup (buildCommitment))

build ::
  (MonadIO m, MonadReader FileScheme m, MonadError [WBPSFailure] m) =>
  UserWalletPublicKey ->
  Input ->
  m Commitment
build userWalletPublicKey input = do
  accountDirectory <- deriveAccountDirectoryFrom userWalletPublicKey
  ensureDir accountDirectory
  randSuffix <- liftIO (encodeHex <$> getRandomBytes 16)
  let tmpPrefix = "build-commitment-tmp-" <> T.unpack randSuffix <> "-"
  tmpRoot <- createTempDir accountDirectory tmpPrefix
  Output {maskedChunks} <- toWBPSFailure =<< runBuildCommitment def tmpRoot input
  let commitment@Commitment {id = commitmentId} = mkCommitment (CommitmentPayload maskedChunks)
  sessionDirectory <- deriveSessionDirectoryFrom userWalletPublicKey commitmentId
  ensureDir sessionDirectory
  renameDir tmpRoot (sessionDirectory </> [reldir|build-commitment-traces|])
  return commitment

data Input = Input
  { ekPowRho :: AffinePoint
  , messageBits :: MessageBits
  }

data Output = Output
  { messageChunks :: MessageLimbs
  , maskedChunks :: MessageLimbs
  }

data Context = Context
  { commitmentLimbSize :: Int
  , nbCommitmentLimbs :: Int
  }

instance Default Context where
  def =
    Context
      { commitmentLimbSize = 254
      , nbCommitmentLimbs = 27
      }

newtype WitnessValues = WitnessValues [String]

instance Aeson.FromJSON WitnessValues where
  parseJSON (Aeson.Object o) = WitnessValues <$> o .: "witness"
  parseJSON (Aeson.Array arr) = WitnessValues <$> traverse Aeson.parseJSON (V.toList arr)
  parseJSON v = AesonTypes.typeMismatch "Array or witness object" v

runBuildCommitment ::
  (MonadIO m, MonadReader FileScheme m) =>
  Context ->
  Path Abs Dir ->
  Input ->
  m (Either String Output)
runBuildCommitment params tmpRoot Input {ekPowRho = AffinePoint {x, y}, ..} = do
  scheme <- ask
  setup <- asks (Setup.buildCommitment . setup)
  Filescheme.BuildCommitmentInternals {input, output, statementOutput} <- compileAndScheme setup
  let inputJson =
        Aeson.object
          [ "in_seed_x" Aeson..= T.pack (show x)
          , "in_seed_y" Aeson..= T.pack (show y)
          , "in_message" Aeson..= messageBits
          ]

  let inputPath = tmpRoot </> input
      statementPath = tmpRoot </> statementOutput
      shellLogsFilepath = BL8.pack $ Path.toFilePath (tmpRoot </> (shellLogs . account $ scheme))
  writeTo inputPath inputJson
  witnessProc <- getGenerateBuildCommitmentWitnessProcess tmpRoot
  liftIO $
    witnessProc
      &!> StdOut
      &> Append shellLogsFilepath
      >> exportStatementAsJSON tmpRoot output statementOutput
        &!> StdOut
  parseOutputs params setup statementPath

exportStatementAsJSON :: Path Abs Dir -> Path Rel File -> Path Rel File -> Proc ()
exportStatementAsJSON directory witness statement =
  Snarkjs.exportStatementAsJSON (toFilePath (directory </> witness)) (toFilePath (directory </> statement))

compileAndScheme ::
  (MonadIO m, MonadReader FileScheme m) =>
  BuildCommitmentSetup ->
  m BuildCommitmentInternals
compileAndScheme Filescheme.BuildCommitmentSetup {wasm, r1cs} = do
  let requiredArtifacts =
        [ wasm
        , r1cs
        ]
  artifactsPresent <-
    liftIO $
      allM doesFileExist requiredArtifacts
  unless artifactsPresent $ do
    compileProc <- compileBuildCommitmentForFileScheme
    liftIO $ compileProc &!> StdOut
  asks (buildCommitmentInternals . demonstration . session . account)

parseOutputs :: MonadIO m => Context -> BuildCommitmentSetup -> Path b t -> m (Either String Output)
parseOutputs params buildCommitmentSetup statementPath = do
  bytes <- liftIO $ BL.readFile (toFilePath statementPath)
  case Aeson.eitherDecode bytes of
    Left err -> pure (Left err)
    Right (WitnessValues wVals) -> do
      let ints = fmap read wVals :: [Integer]
      eIdxs <- getOutputIndices buildCommitmentSetup params
      case eIdxs of
        Left err -> pure (Left err)
        Right (msgIdxs, maskedIdxs) -> do
          let maxIdx = maximum (msgIdxs ++ maskedIdxs)
          if maxIdx >= length ints
            then pure (Left "witness shorter than expected when extracting commitment outputs")
            else
              let messageChunks = MessageLimbs [ints !! i | i <- msgIdxs]
                  maskedChunks = MessageLimbs [ints !! i | i <- maskedIdxs]
               in pure (Right Output {messageChunks, maskedChunks})

getOutputIndices ::
  MonadIO m =>
  BuildCommitmentSetup ->
  Context ->
  m (Either String ([Int], [Int]))
getOutputIndices BuildCommitmentSetup {r1cs} Context {nbCommitmentLimbs} = liftIO $ do
  -- Circom emits a .sym file alongside the r1cs/wasm that maps witness indices to signal names.
  -- This code reads that file line-by-line, splits on commas, and collects indices whose names
  -- start with main.out_message_chunk[ and main.out_masked_chunk[. It sorts these to recover the
  -- order of public outputs and returns those index lists. This lets parseOutputs pull the correct
  -- entries from the exported witness JSON (public.json) without hardcoding the ordering that
  -- snarkjs writes. It's brittle because coupled to the exact signal names and the "main"
  -- component name; a change in circuit naming/layout would require updating the prefixes. But
  -- it's fine for now.
  let symPath = FP.replaceExtension (toFilePath r1cs) "sym"
  symLines <- T.lines <$> TIO.readFile symPath
  let parseLine l =
        case T.splitOn "," l of
          (idxTxt : _ : _ : name : _) -> do
            idx <- readMaybe (T.unpack idxTxt)
            pure (idx, name)
          _ -> Nothing
      entries = mapMaybe parseLine symLines
      msgIdxs = sort [i | (i, name) <- entries, "main.out_message_chunk[" `T.isPrefixOf` name]
      maskedIdxs = sort [i | (i, name) <- entries, "main.out_masked_chunk[" `T.isPrefixOf` name]
  if length msgIdxs /= nbCommitmentLimbs || length maskedIdxs /= nbCommitmentLimbs
    then
      pure
        ( Left
            ( "unexpected number of commitment limbs in .sym: "
                <> show (length msgIdxs, length maskedIdxs)
                <> " expected "
                <> show nbCommitmentLimbs
            )
        )
    else pure (Right (msgIdxs, maskedIdxs))
