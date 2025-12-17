{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module WBPS.Core.Commitment.BuildCommitment (
  BuildCommitmentInput (..),
  BuildCommitmentOutput (..),
  Commitment (..),
  CommitmentPayload (..),
  ComId (..),
  mkCommitment,
  runBuildCommitment,
) where

import Cardano.Crypto.Hash (ByteString)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ask, runReader)
import Crypto.Hash (SHA256, hash)
import Data.Aeson (FromJSON, ToJSON, (.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as AesonTypes
import Data.ByteArray qualified as BA
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Coerce (coerce)
import Data.List (sort, unfoldr)
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Vector qualified as V
import Data.Word (Word8)
import GHC.Generics (Generic)
import Path (toFilePath, (</>))
import Path.IO (doesFileExist, ensureDir, withTempDir)
import Shh (Stream (Append, StdOut), (&!>), (&>))
import System.FilePath qualified as FP
import Text.Read (readMaybe)
import WBPS.Adapter.CardanoCryptoClass.Crypto (FromByteString (fromByteString), Hexadecimal)
import WBPS.Core.FileScheme (
  FileScheme (
    FileScheme,
    accounts,
    buildCommitmentR1CS,
    buildCommitmentWASM,
    shellLogs,
    statementOutput,
    witnessInput,
    witnessOutput
  ),
 )
import WBPS.Core.Keys.ElGamal (AffinePoint (AffinePoint), x, y)
import WBPS.Core.Primitives.Circom (
  BuildCommitmentParams (BuildCommitmentParams),
  compileBuildCommitmentForFileScheme,
  nbCommitmentLimbs,
 )
import WBPS.Core.Primitives.Snarkjs (exportStatementAsJSON)
import WBPS.Core.Primitives.SnarkjsOverFileScheme (getGenerateBuildCommitmentWitnessProcess)

data BuildCommitmentInput = BuildCommitmentInput
  { ekPowRho :: AffinePoint
  , messageBits :: [Int]
  }

data BuildCommitmentOutput = BuildCommitmentOutput
  { messageChunks :: [Integer]
  , maskedChunks :: [Integer]
  }

newtype WitnessValues = WitnessValues [String]

instance Aeson.FromJSON WitnessValues where
  parseJSON (Aeson.Object o) = WitnessValues <$> o .: "witness"
  parseJSON (Aeson.Array arr) = WitnessValues <$> traverse Aeson.parseJSON (V.toList arr)
  parseJSON v = AesonTypes.typeMismatch "Array or witness object" v

newtype ComId = ComId {unComId :: Hexadecimal}
  deriving newtype (Eq, Show, FromJSON, ToJSON)

newtype CommitmentPayload = CommitmentPayload
  { payload :: [Integer]
  }
  deriving newtype (Eq, Show, FromJSON, ToJSON)

data Commitment
  = Commitment {id :: ComId, payload :: CommitmentPayload}
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

mkCommitment :: CommitmentPayload -> Commitment
mkCommitment payload =
  let
    -- Encode each limb as big-endian bytes with length prefix
    -- It takes the [Integer] payload limbs and encodes each as big-endian
    -- bytes with a 1-byte length prefix (len || limb_bytes), concatenates all of them, then hashes
    -- with SHA-256 to get ComId. This is a simple, deterministic framing to avoid ambiguity
    -- between limbs like [1, 23] vs [12, 3], but it's not CBOR and there's no domain separation
    -- or explicit length for the whole list.
    encodeLimb n =
      let bs = integerToBytes n
       in BS.cons (fromIntegral (length bs)) (BS.pack bs)
    flat = BS.concat (map encodeLimb (coerce payload))
    digest = hash @_ @SHA256 flat
    commitmentId = ComId . fromByteString @Hexadecimal $ (BA.convert digest :: ByteString)
   in
    Commitment {id = commitmentId, payload}

integerToBytes :: Integer -> [Word8]
integerToBytes 0 = [0]
integerToBytes n
  | n < 0 = error "integerToBytes: negative input"
  | otherwise = reverse (unfoldr step n)
  where
    step 0 = Nothing
    step k =
      let (q, r) = k `quotRem` 256
       in Just (fromIntegral r, q)

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM p = fmap and . mapM p

runBuildCommitment ::
  (MonadIO m, MonadReader FileScheme m) =>
  BuildCommitmentParams ->
  BuildCommitmentInput ->
  m (Either String BuildCommitmentOutput)
runBuildCommitment params BuildCommitmentInput {ekPowRho = AffinePoint {x, y}, ..} = do
  scheme <- compileAndScheme params
  let inputJson =
        Aeson.object
          [ "in_seed_x" Aeson..= x
          , "in_seed_y" Aeson..= y
          , "in_message" Aeson..= messageBits
          ]
  let accountsDir = accounts scheme

  liftIO $ do
    ensureDir accountsDir
    withTempDir accountsDir "build-commitment" $ \accountDir -> do
      let inputPath = accountDir </> witnessInput scheme
          witnessPath = accountDir </> witnessOutput scheme
          statementPath = accountDir </> statementOutput scheme
          shellLogsFilepath = BL8.pack $ Path.toFilePath (accountDir </> shellLogs scheme)
      ensureDir accountDir
      BL.writeFile (toFilePath inputPath) (Aeson.encode inputJson)
      let witnessProc = runReader (getGenerateBuildCommitmentWitnessProcess accountDir) scheme
      witnessProc &!> StdOut &> Append shellLogsFilepath
      exportStatementAsJSON (toFilePath witnessPath) (toFilePath statementPath)
        &!> StdOut
      parseOutputs scheme statementPath
  where
    compileAndScheme ::
      (MonadIO m, MonadReader FileScheme m) =>
      BuildCommitmentParams ->
      m FileScheme
    compileAndScheme _ = do
      scheme@FileScheme {buildCommitmentWASM, buildCommitmentR1CS} <- ask
      let requiredArtifacts =
            [ buildCommitmentWASM
            , buildCommitmentR1CS
            ]
      artifactsPresent <-
        liftIO $
          allM doesFileExist requiredArtifacts
      unless artifactsPresent $ do
        compileProc <- compileBuildCommitmentForFileScheme
        liftIO $ compileProc &!> StdOut
      pure scheme

    parseOutputs scheme statementPath = do
      bytes <- BL.readFile (toFilePath statementPath)
      case Aeson.eitherDecode bytes of
        Left err -> pure (Left err)
        Right (WitnessValues wVals) -> do
          let ints = fmap read wVals :: [Integer]
          eIdxs <- getOutputIndices scheme params
          case eIdxs of
            Left err -> pure (Left err)
            Right (msgIdxs, maskedIdxs) -> do
              let maxIdx = maximum (msgIdxs ++ maskedIdxs)
              if maxIdx >= length ints
                then pure (Left "witness shorter than expected when extracting commitment outputs")
                else
                  let messageChunks = [ints !! i | i <- msgIdxs]
                      maskedChunks = [ints !! i | i <- maskedIdxs]
                   in pure (Right BuildCommitmentOutput {messageChunks, maskedChunks})

getOutputIndices ::
  MonadIO m =>
  FileScheme ->
  BuildCommitmentParams ->
  m (Either String ([Int], [Int]))
getOutputIndices FileScheme {buildCommitmentR1CS} BuildCommitmentParams {..} = liftIO $ do
  -- Circom emits a .sym file alongside the r1cs/wasm that maps witness indices to signal names.
  -- This code reads that file line-by-line, splits on commas, and collects indices whose names
  -- start with main.out_message_chunk[ and main.out_masked_chunk[. It sorts these to recover the
  -- order of public outputs and returns those index lists. This lets parseOutputs pull the correct
  -- entries from the exported witness JSON (public.json) without hardcoding the ordering that
  -- snarkjs writes. It's brittle because coupled to the exact signal names and the "main"
  -- component name; a change in circuit naming/layout would require updating the prefixes. But
  -- it's fine for now.
  let symPath = FP.replaceExtension (toFilePath buildCommitmentR1CS) "sym"
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
