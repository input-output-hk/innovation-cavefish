-- | Types for prepared demonstration messages, carrying both structured parts
--   and their circuit-ready bit representations.
module WBPS.Core.Session.Demonstration.Artefacts.PreparedMessage (
  PreparedMessage (..),
  MessageParts (..),
  Message (..),
  PublicMessage (..),
  PrivateMessage (..),
  CircuitMessage (..),
  MessageBits (..),
  messageBitsToText,
) where

import Control.Monad (unless)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), withArray)
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Word (Word8)
import GHC.Generics (Generic)
import WBPS.Adapter.Data.Word8 (word8sToText)
import WBPS.Core.Session.Demonstration.Artefacts.Cardano.UnsignedTx (
  AbstractUnsignedTx (AbstractUnsignedTx),
  PrivateTxInputs,
  UnsignedTx (UnsignedTx),
 )

-- | A prepared message pairs the circuit view with the structured parts.
data PreparedMessage = PreparedMessage
  { circuit :: CircuitMessage
  -- ^ Circuit-ready bit representation used by proving.
  , parts :: MessageParts
  -- ^ Structured view split into public and private components.
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Structured message parts derived from the original unsigned transaction.
data MessageParts = MessageParts
  { message :: Message
  -- ^ The full unsigned transaction.
  , public :: PublicMessage
  -- ^ Publicly revealable subset of the transaction.
  , private :: PrivateMessage
  -- ^ Private transaction inputs extracted from the transaction.
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Bit-level view of the message and its public/private split for the circuit.
data CircuitMessage = CircuitMessage
  { message :: MessageBits
  -- ^ Full message bits padded to the circuit size.
  , public :: MessageBits
  -- ^ Public message bits with the private window zeroed.
  , private :: MessageBits
  -- ^ Private message bits extracted from the private window.
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Full unsigned transaction used to derive all message representations.
newtype Message = Message {unMessage :: UnsignedTx} deriving newtype (Eq, Show, FromJSON, ToJSON)

-- | Public, abstract view of an unsigned transaction.
newtype PublicMessage = PublicMessage {unPublicMessage :: AbstractUnsignedTx} deriving newtype (Eq, Show, FromJSON, ToJSON)

-- | Private inputs extracted from an unsigned transaction.
newtype PrivateMessage = PrivateMessage {unPrivateMessage :: PrivateTxInputs} deriving newtype (Eq, Show, FromJSON, ToJSON)

-- | Bit vector encoded as a bytestring of 0/1 values.
newtype MessageBits = MessageBits {unMessageBits :: ByteString} deriving newtype (Eq, Show)

instance ToJSON MessageBits where
  toJSON (MessageBits bits) = toJSON (BS.unpack bits)

instance FromJSON MessageBits where
  parseJSON = withArray "MessageBits" $ \arr -> do
    bits <- traverse parseJSON (V.toList arr) :: Parser [Word8]
    unless (all isBit bits) $
      fail "MessageBits: expected an array of 0/1 values"
    pure (MessageBits (BS.pack bits))
    where
      isBit b = b == 0 || b == 1

messageBitsToText :: MessageBits -> [Text]
messageBitsToText (MessageBits bits) =
  word8sToText (BS.unpack bits)
