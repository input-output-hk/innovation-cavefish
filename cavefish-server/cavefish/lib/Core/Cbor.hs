{-# LANGUAGE LambdaCase #-}

module Core.Cbor (
  serialiseTxAbs,
  encodeTxAbs,
)
where

import Cardano.Api qualified as Api
import Cardano.Api.Shelley qualified as Api.Shelley
import Cardano.Binary qualified as CBOR
import Codec.CBOR.Encoding qualified as E
import Codec.CBOR.Write qualified as Write
import Core.TxAbs (TxAbs (..))
import Data.ByteString (ByteString)
import Data.Set qualified as Set
import Ledger (Slot (..))
import Ledger.Crypto (PubKey (..))
import PlutusLedgerApi.V1 (fromBuiltin)
import PlutusLedgerApi.V1.Bytes (LedgerBytes (LedgerBytes))
import PlutusLedgerApi.V1.Interval qualified as Interval

serialiseTxAbs :: TxAbs Api.ConwayEra -> ByteString
serialiseTxAbs = Write.toStrictByteString . encodeTxAbs

encodeTxAbs :: TxAbs Api.ConwayEra -> E.Encoding
encodeTxAbs TxAbs{..} =
  E.encodeListLen 5
    <> encodeTxOuts outputs
    <> encodeInterval validityInterval
    <> encodeValue absMint
    <> E.encodeInteger absFee
    <> encodePubKeys sigKeys

encodeTxOuts :: [Api.TxOut Api.CtxTx Api.ConwayEra] -> E.Encoding
encodeTxOuts outs =
  E.encodeListLen (fromIntegral (length outs))
    <> foldMap encodeTxOut outs

encodeTxOut :: Api.TxOut Api.CtxTx Api.ConwayEra -> E.Encoding
encodeTxOut out =
  CBOR.toCBOR $ Api.Shelley.toShelleyTxOut Api.Shelley.ShelleyBasedEraConway (Api.toCtxUTxOTxOut out)

encodeInterval :: Interval.Interval Slot -> E.Encoding
encodeInterval (Interval.Interval lower upper) =
  E.encodeListLen 2 <> encodeLower lower <> encodeUpper upper

encodeLower :: Interval.LowerBound Slot -> E.Encoding
encodeLower (Interval.LowerBound ext closed) =
  E.encodeListLen 2 <> encodeExtended ext <> E.encodeBool closed

encodeUpper :: Interval.UpperBound Slot -> E.Encoding
encodeUpper (Interval.UpperBound ext closed) =
  E.encodeListLen 2 <> encodeExtended ext <> E.encodeBool closed

encodeExtended :: Interval.Extended Slot -> E.Encoding
encodeExtended = \case
  Interval.NegInf -> E.encodeWord8 0
  Interval.PosInf -> E.encodeWord8 1
  Interval.Finite (Slot s) -> E.encodeListLen 2 <> E.encodeWord8 2 <> E.encodeInteger s

encodeValue :: Api.Value -> E.Encoding
encodeValue value =
  let pairs = Api.valueToList value
   in E.encodeListLen (fromIntegral (length pairs))
        <> foldMap encodeValueEntry pairs

encodeValueEntry :: (Api.AssetId, Api.Quantity) -> E.Encoding
encodeValueEntry (assetId, Api.Quantity q) =
  E.encodeListLen 2 <> encodeAssetId assetId <> E.encodeInteger q

encodeAssetId :: Api.AssetId -> E.Encoding
encodeAssetId = \case
  Api.AdaAssetId -> E.encodeWord8 0
  Api.AssetId policy assetName ->
    E.encodeListLen 3
      <> E.encodeWord8 1
      <> E.encodeBytes (Api.serialiseToRawBytes policy)
      <> E.encodeBytes (Api.serialiseToRawBytes assetName)

encodePubKeys :: Set.Set PubKey -> E.Encoding
encodePubKeys keys =
  let ordered = Set.toAscList keys
   in E.encodeListLen (fromIntegral (length ordered))
        <> foldMap encodePubKey ordered
 where
  encodePubKey :: PubKey -> E.Encoding
  encodePubKey (PubKey (LedgerBytes bs)) =
    E.encodeBytes (fromBuiltin bs)
