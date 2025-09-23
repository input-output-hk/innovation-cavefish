{-# LANGUAGE OverloadedStrings #-}

module Core.Proof where

import Cardano.Api qualified as Api
import Crypto.Error (CryptoFailable (..))
import Crypto.PubKey.Ed25519 qualified as Ed
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.ByteArray qualified as BA
import Data.ByteArray.Encoding qualified as BAE
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)

domainTag :: ByteString
domainTag = "cavefish/v1"

serializeTxId :: Api.TxId -> ByteString
serializeTxId = Api.serialiseToRawBytes

newtype Proof
  = Proof ByteString
  deriving (Eq, Show, Generic)

instance ToJSON Proof where
  toJSON (Proof d) =
    object
      [ "data" .= renderHex d
      ]

instance FromJSON Proof where
  parseJSON = withObject "Proof" $ \o -> do
    dataHex :: Text <- o .: "data"
    Proof <$> parseHex dataHex

mkProof :: Ed.SecretKey -> Api.TxId -> ByteString -> Proof
mkProof sk txid txAbsHash =
  let message = domainTag <> serializeTxId txid <> txAbsHash
      signature = Ed.sign sk (Ed.toPublic sk) message
   in Proof (BA.convert signature)

-- What the LC would call to verify
verifyProof :: Ed.PublicKey -> Api.TxId -> ByteString -> Proof -> Bool
verifyProof pk txid txAbsHash (Proof dataBs) =
  case Ed.signature dataBs of
    CryptoFailed _ -> False
    CryptoPassed sig ->
      let message = domainTag <> serializeTxId txid <> txAbsHash
       in Ed.verify pk message sig

renderHex :: ByteString -> Text
renderHex = TE.decodeUtf8 . BAE.convertToBase BAE.Base16

parseHex :: Text -> Parser ByteString
parseHex t =
  case BAE.convertFromBase BAE.Base16 (TE.encodeUtf8 t) of
    Left err -> fail err
    Right bs -> pure bs
