module WBPS.Core.ZK.Message (
  Message (..),
  PublicMessage (..),
  MessageBits (..),
  MaxMessageBits (..),
  PreparedMessage (..),
  messageToBits,
  messagePayloadBitLength,
  prepareMessage,
  publicMessageToMessageBits,
  messageBitsToWord8s,
) where

import Cardano.Api qualified as Api
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bits (testBit)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Default (Default (def))
import Data.Word (Word8)
import GHC.Generics (Generic)
import WBPS.Core.Cardano.UnsignedTx (
  AbstractUnsignedTx (AbstractUnsignedTx),
  UnsignedTx (UnsignedTx),
  randomizeTx,
  toAbstractUnsignedTx,
  txUnsigned,
 )

newtype Message = Message {unMessage :: UnsignedTx}
  deriving newtype (Eq, Show, FromJSON, ToJSON)

newtype PublicMessage = PublicMessage {unPublicMessage :: AbstractUnsignedTx}
  deriving newtype (Eq, Show, FromJSON, ToJSON)

newtype MessageBits = MessageBits {unMessageBits :: [Integer]}
  deriving newtype (Eq, Show, FromJSON, ToJSON)

newtype MaxMessageBits = MaxMessageBits {unMaxMessageBits :: Int}

instance Default MaxMessageBits where
  def =
    -- Sized for 800B tx_body + 32B nonce, padded to 27 * 254 bits.
    -- See wbps/README.md for mainnet size distribution stats.
    MaxMessageBits (27 * 254)

data PreparedMessage = PreparedMessage
  { publicMessage :: PublicMessage
  , message :: Message
  , messageBits :: MessageBits
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

messageToBits :: MaxMessageBits -> Message -> MessageBits
messageToBits (MaxMessageBits maxSize) (Message unsignedTx) =
  MessageBits $ take maxSize (payloadBits (Api.serialiseToCBOR (txUnsigned unsignedTx)) ++ repeat 0)

messagePayloadBitLength :: Message -> Int
messagePayloadBitLength (Message unsignedTx) =
  BS.length (Api.serialiseToCBOR (txUnsigned unsignedTx)) * 8

prepareMessage ::
  MonadIO m =>
  UnsignedTx -> m (Either String PreparedMessage)
prepareMessage unsignedTx = do
  randomizedTx <- randomizeTx unsignedTx
  let message = Message randomizedTx
      publicMessage = PublicMessage . toAbstractUnsignedTx . unMessage $ message
      maxMessageBits@(MaxMessageBits maxBits) = def
      actualBits = messagePayloadBitLength message
  pure $
    if actualBits > maxBits
      then Left (txBuiltTooLargeMessage actualBits maxBits)
      else Right PreparedMessage {publicMessage, message, messageBits = messageToBits maxMessageBits message}

publicMessageToMessageBits :: PublicMessage -> MessageBits
publicMessageToMessageBits (PublicMessage (AbstractUnsignedTx unsignedTx)) =
  messageToBits def (Message unsignedTx)

messageBitsToWord8s :: MessageBits -> [Word8]
messageBitsToWord8s (MessageBits bits) = map fromIntegral bits

-- Convert a bytestring to a little-endian bit vector.
payloadBits :: ByteString -> [Integer]
payloadBits bs = concatMap byteToBits (BS.unpack bs)
  where
    byteToBits b =
      [ if testBit b i then 1 else 0
      | i <- [0 .. 7]
      ]

txBuiltTooLargeMessage :: Int -> Int -> String
txBuiltTooLargeMessage actualBits maxBits =
  "TxBuilt is above the maximum sized handled by the configured circuit (tx bits: "
    <> show actualBits
    <> ", max bits: "
    <> show maxBits
    <> ")."
