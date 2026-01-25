-- | Circuit configuration parameters and constants that must stay aligned
--   with the Circom relation and the CBOR layout of Cardano transactions.
--   This module centralizes those invariants to avoid drift between
--   Haskell code and `wbps/setup/relation/relation.circom`.
module WBPS.Core.Setup.Circuit.Parameters (
  CircuitParameters (..),
  CircuitMessageMaxSize (..),
  CircuitTxInputSize (..),
  MessagePrivatePartOffsetBits (..),
  MessagePrivatePartSizeBits (..),
  mkCircuitTxInputSize,
  messagePrivatePartOffset,
  messagePrivatePartSize,
  assertFitsCircuitParameters,
  txPayloadBitLength,
) where

import Cardano.Api qualified as Api
import Control.Monad (when, (<=<))
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString qualified as BS
import Data.Default (Default (def))
import WBPS.Core.Failure (WBPSFailure (TxBuiltTooLarge, TxInputsCountMismatch))
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Cardano.UnsignedTx (UnsignedTx (txUnsigned))

data CircuitParameters = CircuitParameters
  { messageSize :: CircuitMessageMaxSize
  , txInputSize :: CircuitTxInputSize
  }
  deriving (Show, Eq)

instance Default CircuitParameters where
  def = CircuitParameters def def

-- | Maximum message size in bits for the circuit (padded to this size).
newtype CircuitMessageMaxSize = CircuitMessageMaxSize {unCircuitMessageMaxSize :: Int} deriving newtype (Eq, Ord, Show)

instance Default CircuitMessageMaxSize where
  def =
    -- Sized for 857B tx_body + 32B nonce, aligned to 28 * 254 bits.
    -- See wbps/README.md for mainnet size distribution stats.
    CircuitMessageMaxSize (28 * 254)

-- | Count of tx inputs supported by the circuit.
--   This is constrained to the design limit (currently <= 23).
newtype CircuitTxInputSize = CircuitTxInputSize {unCircuitTxInputSize :: Int}
  deriving newtype (Eq, Ord, Show)

-- | Smart constructor enforcing the input-count invariant.
--   Keep this aligned with the Circom circuit parameters.
mkCircuitTxInputSize :: Int -> Either String CircuitTxInputSize
mkCircuitTxInputSize inputCount
  | inputCount <= 0 = Left "CircuitTxInputSize must be > 0."
  | inputCount > maxSupportedInputs = Left "CircuitTxInputSize must be <= 23."
  | otherwise = Right (CircuitTxInputSize inputCount)

instance Default CircuitTxInputSize where
  def =
    -- Circuit for this prototype currently set to handle only one input.
    CircuitTxInputSize 1

-- | Offset (in bits) of the private overlay window within the CBOR bit vector.
newtype MessagePrivatePartOffsetBits = MessagePrivatePartOffsetBits
  { unMessagePrivatePartOffsetBits :: Int
  }
  deriving newtype (Eq, Ord, Show)

-- | Size (in bits) of the private overlay window within the CBOR bit vector.
newtype MessagePrivatePartSizeBits = MessagePrivatePartSizeBits
  { unMessagePrivatePartSizeBits :: Int
  }
  deriving newtype (Eq, Ord, Show)

-- | Bit offset into the CBOR-encoded tx body bit vector where the private overlay begins.
--   Must match the `message_private_part_offset` parameter used to compile
--   `wbps/setup/relation/relation.circom`. Any change requires rebuilding the circuit.
--   We ran `scripts/inspect_tx_inputs_window.hs` today and observed:
--   offset_bits = 24 (offset_bytes = 3) for the current dummy Conway tx shape.
--   Assumption: this offset should not change because tx inputs are the first
--   element in the CBOR-encoded tx body map (key 0).
messagePrivatePartOffset :: MessagePrivatePartOffsetBits
messagePrivatePartOffset = MessagePrivatePartOffsetBits 24

-- | Bit length of the private overlay window within the CBOR-encoded tx body bit vector.
--   Must match the `message_private_part_size` parameter used to compile
--   `wbps/setup/relation/relation.circom`. See `scripts/inspect_tx_inputs_window.hs`
--   to recompute sizes for a given input count.
--   We ran `scripts/inspect_tx_inputs_window.hs` today and observed:
--   size_bits = 320 (size_bytes = 40) for 1 input, with per_input_bytes = 36
--   and overhead_bytes = 4 for N <= 23, so size_bits = 8 * (4 + 36 * N).
--   This implementation only supports N <= 23.
--   NOTE: the 4B overhead is the CBOR tag + array header for N <= 23.
messagePrivatePartSize :: CircuitTxInputSize -> MessagePrivatePartSizeBits
messagePrivatePartSize (CircuitTxInputSize inputCount) =
  MessagePrivatePartSizeBits (bytesToBits totalBytes)
  where
    safeCount
      | inputCount <= maxSupportedInputs = inputCount
      | otherwise = error "messagePrivatePartSize: input count exceeds 23"
    totalBytes =
      messagePrivatePartSetTagBytes
        + cborArrayHeaderBytes safeCount
        + (messagePrivatePartPerInputBytes * safeCount)

-- CBOR tag for set (0xd9 01 02) used by tx inputs in the tx body map.
messagePrivatePartSetTagBytes :: Int
messagePrivatePartSetTagBytes = 3

-- Assumes each TxIx is in the small-int range (0..23), so CBOR encodes the
-- index in a single byte. If any input index is >= 24, CBOR uses additional
-- bytes for the index and the per-input size grows.
messagePrivatePartPerInputBytes :: Int
messagePrivatePartPerInputBytes = 36

-- Design limit: only support tx input counts that fit in a 1-byte CBOR array header.
maxSupportedInputs :: Int
maxSupportedInputs = 23

bytesToBits :: Int -> Int
bytesToBits bytes = bytes * 8

cborArrayHeaderBytes :: Int -> Int
cborArrayHeaderBytes n
  | n <= maxSupportedInputs = 1
  | otherwise = error "cborArrayHeaderBytes: input count exceeds 23"

assertFitsCircuitParameters ::
  (MonadIO m, MonadError [WBPSFailure] m) =>
  CircuitParameters ->
  UnsignedTx ->
  m UnsignedTx
assertFitsCircuitParameters CircuitParameters {messageSize, txInputSize} =
  assertFitsCircuitMessageMaxSize messageSize <=< assertFitsCircuitTxInputSize txInputSize

txPayloadBitLength :: UnsignedTx -> Int
txPayloadBitLength tx =
  BS.length (Api.serialiseToCBOR (txUnsigned tx)) * 8

assertFitsCircuitTxInputSize ::
  (MonadIO m, MonadError [WBPSFailure] m) =>
  CircuitTxInputSize ->
  UnsignedTx ->
  m UnsignedTx
assertFitsCircuitTxInputSize (CircuitTxInputSize maxInputs) tx = do
  let actualInputs = length (Api.txIns (Api.getTxBodyContent (txUnsigned tx)))
  when (actualInputs /= maxInputs) $
    throwError [TxInputsCountMismatch (txInputsCountMismatchMessage actualInputs maxInputs)]
  pure tx

assertFitsCircuitMessageMaxSize ::
  (MonadIO m, MonadError [WBPSFailure] m) =>
  CircuitMessageMaxSize ->
  UnsignedTx ->
  m UnsignedTx
assertFitsCircuitMessageMaxSize (CircuitMessageMaxSize maxBits) tx = do
  let actualBits = txPayloadBitLength tx
  when (actualBits > maxBits) $
    throwError [TxBuiltTooLarge (txBuiltTooLargeMessage actualBits maxBits)]
  pure tx

txBuiltTooLargeMessage :: Int -> Int -> String
txBuiltTooLargeMessage actualBits maxBits =
  "TxBuilt is above the maximum sized handled by the configured circuit (tx bits: "
    <> show actualBits
    <> ", max bits: "
    <> show maxBits
    <> ")."

txInputsCountMismatchMessage :: Int -> Int -> String
txInputsCountMismatchMessage actualInputs expectedInputs =
  "TxBuilt input count does not match the configured circuit (tx inputs: "
    <> show actualInputs
    <> ", expected inputs: "
    <> show expectedInputs
    <> ")."
