module WBPS.Core.Session.Steps.Demonstration.Artefacts.Cardano.UnsignedTx (
  PrivateTxInputs (..),
  UnsignedTx (..),
  AbstractUnsignedTx (..),
  extractPrivateElements,
  randomizeTxAndPadItToCircuitMessageSize,
  txBodyMapByteLength,
) where

import Cardano.Api (FromJSON, ToJSON)
import Cardano.Api qualified as Api
import Cardano.Binary qualified as CBOR
import Cardano.Ledger.Api qualified as Ledger
import Cardano.Ledger.Coin (Coin (Coin), unCoin)
import Cardano.Ledger.Plutus.Data qualified as Plutus
import Cardano.Ledger.TxIn qualified as LedgerTxIn
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Identity (Identity (Identity), runIdentity)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (listToMaybe, maybeToList)
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set (Set)
import Lens.Micro ((&), (.~), (^.))
import PlutusLedgerApi.V1 qualified as PV1

newtype UnsignedTx = UnsignedTx
  { txUnsigned :: Api.TxBody Api.ConwayEra
  }
  deriving newtype (Show, Eq)

newtype PrivateTxInputs = PrivateTxInputs
  { txInputs :: NonEmpty Api.TxIn
  }
  deriving newtype (Show, Eq, ToJSON, FromJSON)

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

extractPrivateElements :: UnsignedTx -> (PrivateTxInputs, AbstractUnsignedTx)
extractPrivateElements (UnsignedTx txBody@(Api.ShelleyTxBody era body scripts scriptData metadata validity)) =
  ( privateTxInputsFromTxBody txBody
  , AbstractUnsignedTx . UnsignedTx $
      Api.ShelleyTxBody era (setInputs mempty body) scripts scriptData metadata validity
  )

privateTxInputsFromTxBody :: Api.TxBody Api.ConwayEra -> PrivateTxInputs
privateTxInputsFromTxBody txBody =
  case NE.nonEmpty (map fst (Api.txIns (Api.getTxBodyContent txBody))) of
    Nothing -> error "toAbstractUnsignedTx: empty tx inputs"
    Just inputs -> PrivateTxInputs inputs

setInputs ::
  Ledger.EraTxBody era =>
  Set LedgerTxIn.TxIn ->
  Ledger.TxBody era ->
  Ledger.TxBody era
setInputs ins = runIdentity . Ledger.inputsTxBodyL (\_ -> Identity ins)

randomizeTxAndPadItToCircuitMessageSize :: MonadIO m => Int -> UnsignedTx -> m UnsignedTx
randomizeTxAndPadItToCircuitMessageSize maxBits (UnsignedTx (Api.ShelleyTxBody Api.ShelleyBasedEraConway body scripts scriptData mAux validity)) = do
  let messageSizeMultipleBits = lcm commitmentLimbSizeBits 8
  if maxBits `mod` messageSizeMultipleBits /= 0
    then
      liftIO . ioError . userError $
        "Circuit message size must be a multiple of "
          <> show messageSizeMultipleBits
          <> " bits, got: "
          <> show maxBits
    else do
      let targetBytes = maxBits `div` 8
          baseTx = UnsignedTx (Api.ShelleyTxBody Api.ShelleyBasedEraConway body scripts scriptData mAux validity)
          baseSizeBytes = txBodySizeBytes baseTx
      if baseSizeBytes == targetBytes
        then pure baseTx
        else
          if baseSizeBytes > targetBytes
            then
              liftIO . ioError . userError $
                "Tx body already exceeds circuit size (tx bytes: "
                  <> show baseSizeBytes
                  <> ", target bytes: "
                  <> show targetBytes
                  <> ")."
            else do
              case padTxBodyOutputsToSize targetBytes of
                Left err ->
                  liftIO . ioError . userError $ err
                Right paddedBody ->
                  pure . UnsignedTx $
                    Api.ShelleyTxBody Api.ShelleyBasedEraConway paddedBody scripts scriptData mAux validity
  where
    commitmentLimbSizeBits :: Int
    commitmentLimbSizeBits = 254

    txBodySizeBytes :: UnsignedTx -> Int
    txBodySizeBytes (UnsignedTx txBody) =
      BS.length (Api.serialiseToCBOR txBody)

    txWithOutputs :: [Ledger.TxOut (Api.ShelleyLedgerEra Api.ConwayEra)] -> UnsignedTx
    txWithOutputs outputs =
      UnsignedTx $
        Api.ShelleyTxBody Api.ShelleyBasedEraConway (setOutputs outputs body) scripts scriptData mAux validity

    setOutputs ::
      [Ledger.TxOut (Api.ShelleyLedgerEra Api.ConwayEra)] ->
      Ledger.TxBody (Api.ShelleyLedgerEra Api.ConwayEra) ->
      Ledger.TxBody (Api.ShelleyLedgerEra Api.ConwayEra)
    setOutputs outputs =
      (& Ledger.outputsTxBodyL .~ StrictSeq.fromList outputs)

    padTxBodyOutputsToSize ::
      Int ->
      Either String (Ledger.TxBody (Api.ShelleyLedgerEra Api.ConwayEra))
    padTxBodyOutputsToSize targetBytes = do
      let outputs = toList (body ^. Ledger.outputsTxBodyL)
      case matchCandidate targetBytes outputs of
        Just paddedOutputs -> Right (setOutputs paddedOutputs body)
        Nothing -> do
          (before, baseOutput, after) <- pickPadOutput outputs
          let baseTxOut = baseOutput
              baseCoin = unCoin (baseTxOut ^. Ledger.coinTxOutL)
          if baseCoin <= 1
            then Left "Tx body output coin too small to fragment for padding."
            else
              let candidates =
                    [ paddedOutputs
                    | extraCount <- [1 .. maxExtraOutputs]
                    , padCoin <- padCoinCandidates
                    , let totalPad = padCoin * toInteger extraCount
                    , totalPad < baseCoin
                    , let candidateOutputs = buildOutputs baseCoin before baseTxOut after extraCount padCoin
                    , paddedOutputs <- maybeToList (matchCandidate targetBytes candidateOutputs)
                    ]
               in case listToMaybe candidates of
                    Just paddedOutputs -> Right (setOutputs paddedOutputs body)
                    Nothing ->
                      Left $
                        "Unable to pad tx body to "
                          <> show targetBytes
                          <> " bytes using output fragmentation or inline datum."
      where
        maxExtraOutputs :: Int
        maxExtraOutputs = 50

        padCoinCandidates :: [Integer]
        padCoinCandidates =
          [ 1
          , 23
          , 24
          , 255
          , 256
          , 65535
          , 65536
          , 1000000
          ]

        pickPadOutput ::
          [Ledger.TxOut (Api.ShelleyLedgerEra Api.ConwayEra)] ->
          Either
            String
            ( [Ledger.TxOut (Api.ShelleyLedgerEra Api.ConwayEra)]
            , Ledger.TxOut (Api.ShelleyLedgerEra Api.ConwayEra)
            , [Ledger.TxOut (Api.ShelleyLedgerEra Api.ConwayEra)]
            )
        pickPadOutput outs =
          case break isAdaOnly outs of
            (_, []) ->
              Left "Tx body has no ADA-only outputs to split for padding."
            (before, baseOutput : after) ->
              Right (before, baseOutput, after)

        isAdaOnly :: Ledger.TxOut (Api.ShelleyLedgerEra Api.ConwayEra) -> Bool
        isAdaOnly out =
          out ^. Ledger.isAdaOnlyTxOutF

        matchCandidate ::
          Int ->
          [Ledger.TxOut (Api.ShelleyLedgerEra Api.ConwayEra)] ->
          Maybe [Ledger.TxOut (Api.ShelleyLedgerEra Api.ConwayEra)]
        matchCandidate target outputs =
          let sizeBytes = txBodySizeBytes (txWithOutputs outputs)
           in if sizeBytes == target
                then Just outputs
                else
                  if sizeBytes < target
                    then padOutputsWithDatum target outputs
                    else Nothing

        padOutputsWithDatum ::
          Int ->
          [Ledger.TxOut (Api.ShelleyLedgerEra Api.ConwayEra)] ->
          Maybe [Ledger.TxOut (Api.ShelleyLedgerEra Api.ConwayEra)]
        padOutputsWithDatum target outputs =
          let baseSize = txBodySizeBytes (txWithOutputs outputs)
           in if baseSize >= target
                then Nothing
                else do
                  (before, datumOut, after) <- pickDatumOutput outputs
                  let maxDatumBytes = target - baseSize
                      candidates =
                        [ outputs'
                        | datumBytes <- [0 .. maxDatumBytes]
                        , let out' = attachInlineDatum datumBytes datumOut
                        , let outputs' = before <> (out' : after)
                        , txBodySizeBytes (txWithOutputs outputs') == target
                        ]
                  listToMaybe candidates

        pickDatumOutput ::
          [Ledger.TxOut (Api.ShelleyLedgerEra Api.ConwayEra)] ->
          Maybe
            ( [Ledger.TxOut (Api.ShelleyLedgerEra Api.ConwayEra)]
            , Ledger.TxOut (Api.ShelleyLedgerEra Api.ConwayEra)
            , [Ledger.TxOut (Api.ShelleyLedgerEra Api.ConwayEra)]
            )
        pickDatumOutput outs =
          case break hasNoDatum outs of
            (_, []) -> Nothing
            (before, baseOutput : after) -> Just (before, baseOutput, after)

        hasNoDatum :: Ledger.TxOut (Api.ShelleyLedgerEra Api.ConwayEra) -> Bool
        hasNoDatum out =
          out ^. Ledger.datumTxOutL == Plutus.NoDatum

        attachInlineDatum ::
          Int ->
          Ledger.TxOut (Api.ShelleyLedgerEra Api.ConwayEra) ->
          Ledger.TxOut (Api.ShelleyLedgerEra Api.ConwayEra)
        attachInlineDatum datumBytes out =
          let plutusData =
                Plutus.Data (PV1.B (BS.replicate datumBytes 0))
              inlineDatum =
                Plutus.Datum (Plutus.dataToBinaryData plutusData)
           in out & Ledger.datumTxOutL .~ inlineDatum

        buildOutputs ::
          Integer ->
          [Ledger.TxOut (Api.ShelleyLedgerEra Api.ConwayEra)] ->
          Ledger.TxOut (Api.ShelleyLedgerEra Api.ConwayEra) ->
          [Ledger.TxOut (Api.ShelleyLedgerEra Api.ConwayEra)] ->
          Int ->
          Integer ->
          [Ledger.TxOut (Api.ShelleyLedgerEra Api.ConwayEra)]
        buildOutputs baseCoin before baseTxOut after extraCount padCoin =
          let totalPad = padCoin * toInteger extraCount
              remainingCoin = baseCoin - totalPad
              baseOut' = baseTxOut & Ledger.coinTxOutL .~ Coin remainingCoin
              padOut = baseTxOut & Ledger.coinTxOutL .~ Coin padCoin
           in before <> (baseOut' : replicate extraCount padOut) <> after

txBodyMapByteLength :: UnsignedTx -> Int
txBodyMapByteLength (UnsignedTx (Api.ShelleyTxBody _ body _ _ _ _)) =
  BS.length (CBOR.serialize' body)
