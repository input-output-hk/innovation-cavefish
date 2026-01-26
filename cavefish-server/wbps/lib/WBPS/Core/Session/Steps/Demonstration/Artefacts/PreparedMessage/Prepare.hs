module WBPS.Core.Session.Steps.Demonstration.Artefacts.PreparedMessage.Prepare (
  prepare,
  recompose,
  publicMessageToPublicPartBits,
  splitMessageBits,
  combineMessageBits,
  toBitsPaddedToMaxSize,
) where

import Cardano.Api qualified as Api
import Control.Monad ((<=<))
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Data.Bits (setBit)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List (foldl')
import Data.Word (Word8)
import GHC.Bits (testBit)
import WBPS.Core.Failure (WBPSFailure (CircuitMessageDecodeFailed))
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Cardano.UnsignedTx (
  AbstractUnsignedTx (AbstractUnsignedTx),
  UnsignedTx (UnsignedTx, txUnsigned),
  extractPrivateElements,
  randomizeTxAndPadItToCircuitMessageSize,
 )
import WBPS.Core.Session.Steps.Demonstration.Artefacts.PreparedMessage (
  CircuitMessage (CircuitMessage, message, private, public),
  Message (Message),
  MessageBits (MessageBits),
  MessageParts (MessageParts, message, private, public),
  PreparedMessage (PreparedMessage, circuit, parts),
  PrivateMessage (PrivateMessage),
  PublicMessage (PublicMessage),
 )
import WBPS.Core.Setup.Circuit.Parameters (
  CircuitMessageMaxSize (CircuitMessageMaxSize),
  CircuitParameters (CircuitParameters, messageSize, txInputSize),
  MessagePrivatePartOffsetBits (MessagePrivatePartOffsetBits),
  MessagePrivatePartSizeBits (MessagePrivatePartSizeBits),
  assertFitsCircuitParameters,
  messagePrivatePartOffset,
  messagePrivatePartSize,
 )

prepare ::
  (MonadIO m, MonadError [WBPSFailure] m) =>
  CircuitParameters ->
  UnsignedTx ->
  m PreparedMessage
prepare params =
  fmap (mkPreparedMessage params . toMessageParts)
    . (assertFitsCircuitParameters params <=< randomizeTxAndPadItToCircuitMessageSize maxBits)
  where
    CircuitMessageMaxSize maxBits = messageSize params

recompose ::
  MonadError [WBPSFailure] m =>
  CircuitParameters ->
  CircuitMessage ->
  m PreparedMessage
recompose params circuitMessage@CircuitMessage {public, private} = do
  let privatePartSize = messagePrivatePartSize (txInputSize params)
      messageBits = combineMessageBits messagePrivatePartOffset privatePartSize public private
  unsignedTx <- decodeUnsignedTx messageBits
  pure PreparedMessage {circuit = circuitMessage, parts = toMessageParts unsignedTx}

mkPreparedMessage :: CircuitParameters -> MessageParts -> PreparedMessage
mkPreparedMessage params parts =
  PreparedMessage {circuit = toCircuitMessage params parts, parts}

toMessageParts :: UnsignedTx -> MessageParts
toMessageParts unsignedTx =
  let (txInputs, abstractTx) = extractPrivateElements unsignedTx
   in MessageParts
        { message = Message unsignedTx
        , public = PublicMessage abstractTx
        , private = PrivateMessage txInputs
        }

toCircuitMessage :: CircuitParameters -> MessageParts -> CircuitMessage
toCircuitMessage CircuitParameters {messageSize, txInputSize} MessageParts {..} =
  let messageBits = toBitsPaddedToMaxSize messageSize message
      privatePartSize = messagePrivatePartSize txInputSize
      (publicBits, privateBits) = splitMessageBits messagePrivatePartOffset privatePartSize messageBits
   in CircuitMessage
        { message = messageBits
        , public = publicBits
        , private = privateBits
        }

publicMessageToPublicPartBits ::
  CircuitParameters ->
  PublicMessage ->
  MessageBits
publicMessageToPublicPartBits params (PublicMessage (AbstractUnsignedTx unsignedTx)) =
  let MessageBits publicBits = toBitsPaddedToMaxSize (messageSize params) (Message unsignedTx)
      MessagePrivatePartOffsetBits offsetBits = messagePrivatePartOffset
      MessagePrivatePartSizeBits privatePartBits = messagePrivatePartSize (txInputSize params)
      prefix = BS.take offsetBits publicBits
      suffix = BS.drop (offsetBits + emptyInputsBits) publicBits
      expanded = prefix <> BS.replicate privatePartBits 0 <> suffix
   in MessageBits (BS.take (BS.length publicBits) expanded)

emptyInputsBits :: Int
emptyInputsBits =
  -- CBOR set tag (3B) + empty array header (1B).
  4 * 8

-- The private overlay window is defined by circuit parameters (bit offset/size).
-- Keep these values in sync with the parameters used to compile
-- `wbps/setup/relation/relation.circom`.
splitMessageBits :: MessagePrivatePartOffsetBits -> MessagePrivatePartSizeBits -> MessageBits -> (MessageBits, MessageBits)
splitMessageBits (MessagePrivatePartOffsetBits offsetBits) (MessagePrivatePartSizeBits sizeBits) (MessageBits bits) =
  let privateBits = BS.take sizeBits (BS.drop offsetBits bits)
      publicBits =
        BS.take offsetBits bits
          <> BS.replicate sizeBits 0
          <> BS.drop (offsetBits + sizeBits) bits
   in (MessageBits publicBits, MessageBits privateBits)

combineMessageBits :: MessagePrivatePartOffsetBits -> MessagePrivatePartSizeBits -> MessageBits -> MessageBits -> MessageBits
combineMessageBits (MessagePrivatePartOffsetBits offsetBits) (MessagePrivatePartSizeBits sizeBits) (MessageBits publicBits) (MessageBits privateBits) =
  let prefix = BS.take offsetBits publicBits
      suffix = BS.drop (offsetBits + sizeBits) publicBits
      window = BS.take sizeBits privateBits
   in MessageBits (prefix <> window <> suffix)

decodeUnsignedTx :: MonadError [WBPSFailure] m => MessageBits -> m UnsignedTx
decodeUnsignedTx messageBits =
  case decodeUnsignedTxFromBits messageBits of
    Left err -> throwError [CircuitMessageDecodeFailed err]
    Right unsignedTx -> pure unsignedTx

decodeUnsignedTxFromBits :: MessageBits -> Either String UnsignedTx
decodeUnsignedTxFromBits bits = do
  payload <- messageBitsToBytes bits
  txBody <- decodeTxBody payload
  pure (UnsignedTx txBody)

decodeTxBody :: ByteString -> Either String (Api.TxBody Api.ConwayEra)
decodeTxBody = go
  where
    go bs =
      case Api.deserialiseFromCBOR (Api.AsTxBody Api.AsConwayEra) bs of
        Right txBody -> Right txBody
        Left err ->
          case BS.unsnoc bs of
            Just (initBytes, 0) -> go initBytes
            _ -> Left (show err)

messageBitsToBytes :: MessageBits -> Either String ByteString
messageBitsToBytes (MessageBits bits) =
  let totalBits = BS.length bits
      fullBitsLen = totalBits - (totalBits `mod` 8)
      (fullBits, paddingBits) = BS.splitAt fullBitsLen bits
   in if BS.any (/= 0) paddingBits
        then Left "Circuit message bits have non-zero padding."
        else Right (BS.pack (bitsToBytes (BS.unpack fullBits)))

bitsToBytes :: [Word8] -> [Word8]
bitsToBytes [] = []
bitsToBytes bits =
  let (chunk, rest) = splitAt 8 bits
   in if length chunk < 8
        then []
        else bitsToByte chunk : bitsToBytes rest

bitsToByte :: [Word8] -> Word8
bitsToByte bits =
  foldl' setBitIf 0 (zip [0 .. 7] bits)
  where
    setBitIf acc (i, bit) =
      if bit == 0 then acc else setBit acc i

toBitsPaddedToMaxSize :: CircuitMessageMaxSize -> Message -> MessageBits
toBitsPaddedToMaxSize (CircuitMessageMaxSize maxSize) (Message unsignedTx) =
  let bits = payloadBits (Api.serialiseToCBOR (txUnsigned unsignedTx))
      padding = max 0 (maxSize - BS.length bits)
   in MessageBits (BS.take maxSize (bits <> BS.replicate padding 0))

-- Convert a bytestring to a little-endian bit vector.
payloadBits :: ByteString -> ByteString
payloadBits bs = BS.pack (concatMap byteToBits (BS.unpack bs))
  where
    byteToBits b =
      [ if testBit b i then 1 else 0
      | i <- [0 .. 7]
      ]
