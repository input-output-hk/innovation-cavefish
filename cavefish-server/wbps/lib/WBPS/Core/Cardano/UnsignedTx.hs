module WBPS.Core.Cardano.UnsignedTx (
  UnsignedTx (..),
  AbstractUnsignedTx (..),
  toAbstractUnsignedTx,
  randomizeTx,
) where

import Cardano.Api (FromJSON, ToJSON)
import Cardano.Api qualified as Api
import Cardano.Ledger.Api qualified as Ledger
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Identity (Identity (Identity), runIdentity)
import Crypto.Random (getRandomBytes)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.Map.Strict qualified as Map
import Data.Word (Word64)

newtype UnsignedTx = UnsignedTx
  { txUnsigned :: Api.TxBody Api.ConwayEra
  }
  deriving newtype (Show, Eq)

newtype AbstractUnsignedTx = AbstractUnsignedTx
  { abstractTxUnsigned :: UnsignedTx
  }
  deriving newtype (Show, Eq, ToJSON, FromJSON)

instance ToJSON UnsignedTx where
  toJSON (UnsignedTx body) =
    Aeson.toJSON (Api.serialiseToTextEnvelope Nothing body)

instance FromJSON UnsignedTx where
  parseJSON v = do
    envelope <- Aeson.parseJSON v
    case Api.deserialiseFromTextEnvelope @(Api.TxBody Api.ConwayEra) envelope of
      Left err -> fail (show err)
      Right body -> pure (UnsignedTx body)

toAbstractUnsignedTx :: UnsignedTx -> AbstractUnsignedTx
toAbstractUnsignedTx (UnsignedTx (Api.ShelleyTxBody era body scripts scriptData metadata validity)) =
  AbstractUnsignedTx . UnsignedTx $
    Api.ShelleyTxBody era (setInputs mempty body) scripts scriptData metadata validity
  where
    setInputs ins = runIdentity . Ledger.inputsTxBodyL (\_ -> Identity ins)

-- should add aux in metadata
randomizeTx :: MonadIO m => UnsignedTx -> m UnsignedTx
randomizeTx (UnsignedTx (Api.ShelleyTxBody Api.ShelleyBasedEraConway body scripts scriptData mAux validity)) = do
  auxValue <- liftIO $ getRandomBytes 16
  let auxData' = addAuxMetadata auxValue mAux
  pure . UnsignedTx $ Api.ShelleyTxBody Api.ShelleyBasedEraConway body scripts scriptData auxData' validity

cavefishAuxLabel :: Word64
cavefishAuxLabel = 1991

addAuxMetadata ::
  ByteString ->
  Maybe (Ledger.TxAuxData (Api.ShelleyLedgerEra Api.ConwayEra)) ->
  Maybe (Ledger.TxAuxData (Api.ShelleyLedgerEra Api.ConwayEra))
addAuxMetadata randomBytes = \case
  Nothing ->
    Api.toAuxiliaryData Api.ShelleyBasedEraConway (Api.TxMetadataInEra Api.ShelleyBasedEraConway singleEntryMetadata) Api.TxAuxScriptsNone
  Just auxData ->
    let updatedMetadata =
          Map.insert cavefishAuxLabel (Api.toShelleyMetadatum auxEntry) (Ledger.atadMetadata auxData)
        existingScripts = toList (Ledger.getAlonzoTxAuxDataScripts auxData)
     in Just (Ledger.mkAlonzoTxAuxData updatedMetadata existingScripts)
  where
    singleEntryMetadata =
      Api.makeTransactionMetadata (Map.singleton cavefishAuxLabel auxEntry)
    auxEntry =
      Api.TxMetaMap
        [ (Api.TxMetaText "cavefish_auxilaire", Api.TxMetaBytes randomBytes)
        ]
