module WBPS.Core.Session.Steps.Submitting.Artefacts.TxSignature (
  TxSignature (..),
) where

import Cardano.Api (ConwayEra, KeyWitness)
import Cardano.Api qualified as Api
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson

newtype TxSignature = TxSignature {unTxSignature :: KeyWitness ConwayEra}
  deriving (Eq, Show)

instance ToJSON TxSignature where
  toJSON (TxSignature signature) =
    Aeson.toJSON (Api.serialiseToTextEnvelope Nothing signature)

instance FromJSON TxSignature where
  parseJSON v = do
    envelope <- Aeson.parseJSON v
    case Api.deserialiseFromTextEnvelope @(KeyWitness ConwayEra) envelope of
      Left err -> fail (show err)
      Right signature -> pure (TxSignature signature)
