module WBPS.Core.Session.Steps.Submitting.Artefacts.SubmittedTx (
  SubmitTx,
  SignedTx (..),
  getTxId,
  mkSignedTx,
  SubmittedTx (..),
) where

import Cardano.Api (FromJSON, ToJSON)
import Cardano.Api qualified as Api
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Keys qualified as LedgerKeys
import Control.Monad.Except (MonadError, throwError)
import Data.Aeson qualified as Aeson
import WBPS.Core.Failure (WBPSFailure (SessionSubmittingFailed))
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Cardano.UnsignedTx (
  UnsignedTx (UnsignedTx),
 )
import WBPS.Core.Session.Steps.Submitting.Artefacts.TxSignature (TxSignature (TxSignature))

type SubmitTx m = SignedTx -> m ()

newtype SignedTx = SignedTx {unSignedTx :: Api.Tx Api.ConwayEra}
  deriving newtype (Eq, Show)

getTxId :: SignedTx -> Api.TxId
getTxId (SignedTx tx) = Api.getTxId (Api.getTxBody tx)

mkSignedTx ::
  MonadError [WBPSFailure] m =>
  TxSignature ->
  UnsignedTx ->
  m SignedTx
mkSignedTx (TxSignature witness) (UnsignedTx txBody) = do
  verifyTxWitness txBody witness
  case Api.makeSignedTransaction [witness] txBody of
    tx ->
      if witness `elem` Api.getTxWitnesses tx
        then pure (SignedTx tx)
        else throwError [SessionSubmittingFailed "Signed transaction missing expected witness."]

verifyTxWitness ::
  MonadError [WBPSFailure] m =>
  Api.TxBody Api.ConwayEra ->
  Api.KeyWitness Api.ConwayEra ->
  m ()
verifyTxWitness txBody witness =
  case (txBody, witness) of
    ( Api.ShelleyTxBody Api.ShelleyBasedEraConway ledgerBody _ _ _ _
      , Api.ShelleyKeyWitness Api.ShelleyBasedEraConway (LedgerKeys.WitVKey vkey signed)
      ) ->
        let txHash = Ledger.extractHash (Ledger.hashAnnotated ledgerBody)
         in if LedgerKeys.verifySignedDSIGN vkey txHash signed
              then pure ()
              else throwError [SessionSubmittingFailed "Tx witness verification failed: signature does not match tx body."]
    _ ->
      throwError [SessionSubmittingFailed "Tx witness verification failed: unexpected witness or era."]

newtype SubmittedTx = SubmittedTx {unSubmittedTx :: Api.Tx Api.ConwayEra}
  deriving newtype (Eq, Show)

instance ToJSON SubmittedTx where
  toJSON (SubmittedTx tx) =
    Aeson.toJSON (Api.serialiseToTextEnvelope Nothing tx)

instance FromJSON SubmittedTx where
  parseJSON v = do
    envelope <- Aeson.parseJSON v
    case Api.deserialiseFromTextEnvelope @(Api.Tx Api.ConwayEra) envelope of
      Left err -> fail (show err)
      Right tx -> pure (SubmittedTx tx)
