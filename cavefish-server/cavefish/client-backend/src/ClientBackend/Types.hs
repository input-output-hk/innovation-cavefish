{-# LANGUAGE DeriveGeneric #-}

module ClientBackend.Types where

import Cardano.Api qualified as Api
import Client.Mock (decodeHex)
import Core.Api.Messages (CommitReq)
import Core.Api.State (ClientId)
import Core.Intent (ChangeDelta, IntentW)
import Core.PaymentProof (ProofResult)
import Core.Proof (renderHex)
import Core.SP.AskSubmission qualified as AskSubmission
import Core.SP.DemonstrateCommitment qualified as DemonstrateCommitment
import Core.TxAbs (TxAbs)
import Crypto.Error (CryptoFailable (CryptoFailed, CryptoPassed))
import Crypto.PubKey.Ed25519 qualified as Ed
import Data.Aeson (KeyValue ((.=)), withObject)
import Data.Aeson.Types (FromJSON (parseJSON), ToJSON (toJSON), object, (.:))
import Data.ByteArray qualified as BA
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)

-- TODO WG: I don't like PrepareResp as an input here
data VerifyReq = VerifyReq
  { publicKey :: Ed.PublicKey
  , prepared :: DemonstrateCommitment.Outputs
  , proofResult :: ProofResult
  }
  deriving (Eq, Show, Generic)

instance ToJSON VerifyReq where
  toJSON VerifyReq {publicKey, prepared, proofResult} =
    object
      [ "publicKey" .= renderHex (BA.convert publicKey)
      , "prepared" .= prepared
      , "proofResult" .= proofResult
      ]

instance FromJSON VerifyReq where
  parseJSON = withObject "VerifyReq" $ \o -> do
    pkHex :: Text <- o .: "publicKey"
    pkBytes <- either (fail . Text.unpack) pure (decodeHex "public key" pkHex)
    pk <- case Ed.publicKey pkBytes of
      CryptoPassed v -> pure v
      CryptoFailed err -> fail ("invalid public key: " <> show err)
    prepared <- o .: "prepared"
    proofResult <- o .: "proofResult"
    pure VerifyReq {publicKey = pk, prepared, proofResult}

newtype VerifyResp = VerifyResp (Either Text ()) deriving (Eq, Show, Generic)

instance ToJSON VerifyResp

instance FromJSON VerifyResp

data SatisfiesReq = SatisfiesReq
  { intent :: IntentW
  , txAbs :: TxAbs Api.ConwayEra
  , changeDelta :: ChangeDelta
  }
  deriving (Eq, Show, Generic)

instance ToJSON SatisfiesReq

instance FromJSON SatisfiesReq

newtype SatisfiesResp = SatisfiesResp (Either Text Bool)
  deriving (Eq, Show, Generic)

instance ToJSON SatisfiesResp

instance FromJSON SatisfiesResp

newtype RegisterHelperReq = RegisterHelperReq
  { secretKey :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON RegisterHelperReq

instance FromJSON RegisterHelperReq

data RegisterHelperPayload = RegisterHelperPayload
  { userPublicKey :: Text
  , clientSecret :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON RegisterHelperPayload

instance FromJSON RegisterHelperPayload

newtype RegisterHelperResp = RegisterHelperResp (Either Text RegisterHelperPayload)
  deriving (Eq, Show, Generic)

instance ToJSON RegisterHelperResp

instance FromJSON RegisterHelperResp

data PrepareHelperReq = PrepareHelperReq
  { clientId :: ClientId
  , intent :: IntentW
  }
  deriving (Eq, Show, Generic)

instance ToJSON PrepareHelperReq

instance FromJSON PrepareHelperReq

newtype PrepareHelperResp = PrepareHelperResp (Either Text DemonstrateCommitment.Inputs)
  deriving (Eq, Show, Generic)

instance ToJSON PrepareHelperResp

instance FromJSON PrepareHelperResp

newtype CommitHelperReq = CommitHelperReq
  { txId :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON CommitHelperReq

instance FromJSON CommitHelperReq

data CommitHelperPayload = CommitHelperPayload
  { commitReq :: CommitReq
  , littleR :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON CommitHelperPayload

instance FromJSON CommitHelperPayload

newtype CommitHelperResp = CommitHelperResp (Either Text CommitHelperPayload)
  deriving (Eq, Show, Generic)

instance ToJSON CommitHelperResp

instance FromJSON CommitHelperResp

data FinaliseHelperReq = FinaliseHelperReq
  { secretKey :: Text
  , helperPrepareResp :: DemonstrateCommitment.Outputs
  }
  deriving (Eq, Show, Generic)

instance ToJSON FinaliseHelperReq

instance FromJSON FinaliseHelperReq

newtype FinaliseHelperResp = FinaliseHelperResp (Either Text AskSubmission.Inputs)
  deriving (Eq, Show, Generic)

instance ToJSON FinaliseHelperResp

instance FromJSON FinaliseHelperResp

data PayToOutputReq = PayToOutputReq
  { amount :: Integer
  , address :: Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON PayToOutputReq

instance FromJSON PayToOutputReq

data PayToIntentReq = PayToIntentReq
  { payToOutputs :: NonEmpty PayToOutputReq
  , spendFromAddress :: Maybe Text
  , changeToAddress :: Maybe Text
  , payToMaxFee :: Maybe Integer
  , payToMaxInterval :: Maybe Integer
  }
  deriving (Eq, Show, Generic)

instance ToJSON PayToIntentReq

instance FromJSON PayToIntentReq

newtype IntentHelperResp = IntentHelperResp (Either Text IntentW)
  deriving (Eq, Show, Generic)

instance ToJSON IntentHelperResp

instance FromJSON IntentHelperResp

newtype DemoAddressesResp = DemoAddressesResp
  { addresses :: [Text]
  }
  deriving (Eq, Show, Generic)

instance ToJSON DemoAddressesResp

instance FromJSON DemoAddressesResp
