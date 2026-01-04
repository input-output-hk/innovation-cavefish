{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module WBPS.Core.Session.Commitment.Build (
  build,
  Context (..),
  Input (..),
) where

import Control.Monad (unless)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask, asks, runReader)
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
import Path (Path, toFilePath, (</>))
import Path.IO (doesFileExist, ensureDir, withTempDir)
import Shh (Stream (Append, StdOut), (&!>), (&>))
import System.FilePath qualified as FP
import Text.Read (readMaybe)
import WBPS.Core.Failure (
  RegistrationFailed,
  toWBPSFailure,
 )
import WBPS.Core.FileScheme (
  Account (session, shellLogs),
  BuildCommitment (input, output, statementOutput),
  BuildCommitmentSetup (BuildCommitmentSetup, r1cs, wasm),
  FileScheme (account, accounts, setup),
 )
import WBPS.Core.FileScheme qualified as Filescheme
import WBPS.Core.FileScheme qualified as Session (Session (commitment))
import WBPS.Core.FileScheme qualified as Setup (Setup (commitment))
import WBPS.Core.Keys.ElGamal (AffinePoint (AffinePoint), x, y)
import WBPS.Core.Primitives.Circom (
  compileBuildCommitmentForFileScheme,
 )
import WBPS.Core.Primitives.Snarkjs (exportStatementAsJSON)
import WBPS.Core.Primitives.SnarkjsOverFileScheme (getGenerateBuildCommitmentWitnessProcess)
import WBPS.Core.Session.Commitment (
  Commitment,
  CommitmentPayload (CommitmentPayload),
  mkCommitment,
 )
import WBPS.Core.ZK.Message (
  MessageBits (MessageBits, unMessageBits),
 )

build ::
  (MonadIO m, MonadReader FileScheme m, MonadError [RegistrationFailed] m) =>
  Input ->
  m Commitment
build input = do
  Output {maskedChunks} <- toWBPSFailure =<< runBuildCommitment def input
  return $ mkCommitment (CommitmentPayload maskedChunks)

data Input = Input
  { ekPowRho :: AffinePoint
  , messageBits :: MessageBits
  }

data Output = Output
  { messageChunks :: MessageBits
  , maskedChunks :: MessageBits
  }

data Context = Context
  { commitmentLimbSize :: Int
  , nbCommitmentLimbs :: Int
  }

instance Default Context where
  def =
    Context
      { commitmentLimbSize = 252
      , nbCommitmentLimbs = 522
      }

newtype WitnessValues = WitnessValues [String]

instance Aeson.FromJSON WitnessValues where
  parseJSON (Aeson.Object o) = WitnessValues <$> o .: "witness"
  parseJSON (Aeson.Array arr) = WitnessValues <$> traverse Aeson.parseJSON (V.toList arr)
  parseJSON v = AesonTypes.typeMismatch "Array or witness object" v

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM p = fmap and . mapM p

runBuildCommitment ::
  (MonadIO m, MonadReader FileScheme m) =>
  Context ->
  Input ->
  m (Either String Output)
runBuildCommitment params Input {ekPowRho = AffinePoint {x, y}, ..} = do
  scheme <- ask
  setup <- asks (Setup.commitment . setup)
  Filescheme.BuildCommitment {input, output, statementOutput} <- compileAndScheme setup
  let inputJson =
        Aeson.object
          [ "in_seed_x" Aeson..= x
          , "in_seed_y" Aeson..= y
          , "in_message" Aeson..= unMessageBits messageBits
          ]
  let accountsDir = accounts scheme

  liftIO $ do
    ensureDir accountsDir
    withTempDir accountsDir "build-commitment" $ \accountDir -> do
      let inputPath = accountDir </> input
          witnessPath = accountDir </> output
          statementPath = accountDir </> statementOutput
          shellLogsFilepath = BL8.pack $ Path.toFilePath (accountDir </> (shellLogs . account $ scheme))
      ensureDir accountDir
      BL.writeFile (toFilePath inputPath) (Aeson.encode inputJson)
      let witnessProc = runReader (getGenerateBuildCommitmentWitnessProcess accountDir) scheme
      witnessProc &!> StdOut &> Append shellLogsFilepath
      exportStatementAsJSON (toFilePath witnessPath) (toFilePath statementPath)
        &!> StdOut
      parseOutputs params setup statementPath

compileAndScheme ::
  (MonadIO m, MonadReader FileScheme m) =>
  BuildCommitmentSetup ->
  m BuildCommitment
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
  asks (Session.commitment . session . account)

parseOutputs :: Context -> BuildCommitmentSetup -> Path b t -> IO (Either String Output)
parseOutputs params buildCommitmentSetup statementPath = do
  bytes <- BL.readFile (toFilePath statementPath)
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
              let messageChunks = MessageBits [ints !! i | i <- msgIdxs]
                  maskedChunks = MessageBits [ints !! i | i <- maskedIdxs]
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
