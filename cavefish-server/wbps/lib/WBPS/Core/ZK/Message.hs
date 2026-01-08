module WBPS.Core.ZK.Message (
  Message (..),
  PublicMessage (..),
  MessageBits (..),
  MaxMessageBits (..),
  messageToBits,
  publicMessageToMessageBits,
  messageBitsToWord8s,
) where

import Cardano.Api qualified as Api
import Data.Aeson (FromJSON, ToJSON)
import Data.Bits (testBit)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Default (Default (def))
import Data.Word (Word8)
import WBPS.Core.Cardano.UnsignedTx (
  AbstractUnsignedTx (AbstractUnsignedTx),
  UnsignedTx (UnsignedTx),
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
    MaxMessageBits 131544

messageToBits :: MaxMessageBits -> Message -> MessageBits
messageToBits (MaxMessageBits maxSize) (Message unsignedTx) =
  MessageBits $ take maxSize (payloadBits (Api.serialiseToCBOR (txUnsigned unsignedTx)) ++ repeat 0)

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
