{-# LANGUAGE LambdaCase #-}

module Core.Cbor (
  serialiseTx,
  serialiseTxBody,
  serialiseTxBodyMasked,
  serialiseTxAbs,
  encodeTxAbs,
  maskTxBody,
  WitnessBundle (..),
  mkWitnessBundle,
  serialiseWitnessBundle,
  serialiseClientWitnessBundle,
  ClientWitnessBundle (..),
  deserialiseClientWitnessBundle,
) where

import Cardano.Api qualified as Api
import Cardano.Api.Shelley qualified as Api.Shelley
import Cardano.Binary qualified as CBOR
import Codec.CBOR.Encoding qualified as E
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Term (
  Term (TBytes, TBytesI, TInt, TInteger, TList, TListI, TMap, TMapI, TTagged),
  decodeTerm,
  encodeTerm,
 )
import Codec.CBOR.Write qualified as Write
import Core.Pke (PkeCiphertext, deserialiseCiphertext, serialiseCiphertext)
import Core.TxAbs (TxAbs (TxAbs, absFee, absMint, outputs, sigKeys, validityInterval))
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Ledger (Slot (Slot))
import Ledger.Crypto (PubKey (PubKey))
import PlutusLedgerApi.V1 (fromBuiltin)
import PlutusLedgerApi.V1.Bytes (LedgerBytes (LedgerBytes))
import PlutusLedgerApi.V1.Interval qualified as Interval

serialiseTxAbs :: TxAbs Api.ConwayEra -> ByteString
serialiseTxAbs = Write.toStrictByteString . encodeTxAbs

serialiseTx :: Api.Tx Api.ConwayEra -> ByteString
serialiseTx = Api.serialiseToCBOR

serialiseTxBody :: Api.Tx Api.ConwayEra -> ByteString
serialiseTxBody = Api.serialiseToCBOR . Api.getTxBody

serialiseTxBodyMasked ::
  Api.Tx Api.ConwayEra ->
  Either Text ByteString
serialiseTxBodyMasked tx =
  maskTxBody (serialiseTxBody tx)

encodeTxAbs :: TxAbs Api.ConwayEra -> E.Encoding
encodeTxAbs TxAbs {..} =
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

data WitnessBundle = WitnessBundle
  { txId :: ByteString
  , txAbs :: ByteString
  , txBody :: ByteString
  , txBodyMasked :: ByteString
  , observer :: ByteString
  , auxNonce :: ByteString
  , encryptedTx :: PkeCiphertext
  }

mkWitnessBundle ::
  Api.Tx Api.ConwayEra ->
  TxAbs Api.ConwayEra ->
  ByteString ->
  ByteString ->
  PkeCiphertext ->
  Either Text WitnessBundle
mkWitnessBundle tx txAbs observer aux encrypted = do
  let rawBody = serialiseTxBody tx
  maskedBody <- maskTxBody rawBody
  pure
    WitnessBundle
      { txId = Api.serialiseToRawBytes (Api.getTxId (Api.getTxBody tx))
      , txAbs = serialiseTxAbs txAbs
      , txBody = rawBody
      , txBodyMasked = maskedBody
      , observer
      , auxNonce = aux
      , encryptedTx = encrypted
      }

encodeWitnessBundle :: WitnessBundle -> E.Encoding
encodeWitnessBundle WitnessBundle {txId, txAbs, txBody, txBodyMasked, observer, auxNonce, encryptedTx} =
  E.encodeListLen 7
    <> E.encodeBytes txId
    <> E.encodeBytes txAbs
    <> E.encodeBytes txBody
    <> E.encodeBytes txBodyMasked
    <> E.encodeBytes observer
    <> E.encodeBytes auxNonce
    <> E.encodeBytes (serialiseCiphertext encryptedTx)

serialiseWitnessBundle :: WitnessBundle -> ByteString
serialiseWitnessBundle =
  Write.toStrictByteString . encodeWitnessBundle

serialiseClientWitnessBundle :: WitnessBundle -> ByteString
serialiseClientWitnessBundle WitnessBundle {txId, txAbs, txBodyMasked, auxNonce, encryptedTx} =
  Write.toStrictByteString $
    E.encodeListLen 5
      <> E.encodeBytes txId
      <> E.encodeBytes txAbs
      <> E.encodeBytes txBodyMasked
      <> E.encodeBytes auxNonce
      <> E.encodeBytes (serialiseCiphertext encryptedTx)

data ClientWitnessBundle = ClientWitnessBundle
  { cwbTxId :: ByteString
  , cwbTxAbs :: ByteString
  , cwbTxBodyMasked :: ByteString
  , cwbAuxNonce :: ByteString
  , cwbCiphertext :: PkeCiphertext -- This is `comTx`
  }

deserialiseClientWitnessBundle :: ByteString -> Either Text ClientWitnessBundle
deserialiseClientWitnessBundle bytes = do
  (rest, term) <- first (T.pack . show) (deserialiseFromBytes decodeTerm (BL.fromStrict bytes))
  if BL.null rest
    then fromTerm term
    else Left "witness bundle: unexpected trailing bytes"
  where
    fromTerm :: Term -> Either Text ClientWitnessBundle
    fromTerm (TList elems) = decodeElems elems
    fromTerm (TListI elems) = decodeElems elems
    fromTerm _ = Left "witness bundle: unexpected CBOR shape"

    decodeElems :: [Term] -> Either Text ClientWitnessBundle
    decodeElems [txIdTerm, txAbsTerm, maskedBodyTerm, auxTerm, cipherTerm] = do
      txIdBytes <- expectBytes "tx id" txIdTerm
      txAbsBytes <- expectBytes "tx abs" txAbsTerm
      maskedBody <- expectBytes "masked tx body" maskedBodyTerm
      auxBytes <- expectBytes "aux nonce" auxTerm
      cipherBytes <- expectBytes "ciphertext" cipherTerm
      ciphertext <- deserialiseCiphertext cipherBytes
      pure
        ClientWitnessBundle
          { cwbTxId = txIdBytes
          , cwbTxAbs = txAbsBytes
          , cwbTxBodyMasked = maskedBody
          , cwbAuxNonce = auxBytes
          , cwbCiphertext = ciphertext
          }
    decodeElems _ = Left "witness bundle: invalid element count"

    expectBytes :: Text -> Term -> Either Text ByteString
    expectBytes label = \case
      TBytes bs -> Right bs
      TBytesI lbs -> Right (BL.toStrict lbs)
      _ -> Left ("witness bundle: expected bytes for " <> label)

maskTxBody :: ByteString -> Either Text ByteString
maskTxBody bytes = do
  term <- decodeOnly bytes
  pure . Write.toStrictByteString . encodeTerm . maskTxBodyTerm $ term

decodeOnly :: ByteString -> Either Text Term
decodeOnly bs = do
  (rest, term) <- first (T.pack . show) (deserialiseFromBytes decodeTerm (BL.fromStrict bs))
  if BL.null rest
    then pure term
    else Left "maskTxBody: leftover bytes after decoding"

-- | Zero out the body according to the guidance from the paper:
--   "At the minimum the SP hides the inputs to the
--   transaction, i.e., the references to UTxO objects which are present in the ledger and required to
--   cover the transaction."
--   - Section 7.0
--
--   The Conway CDDL specification for the tx body:
--
--   transaction_body =
--   {   0  : set<transaction_input>
--   ,   1  : [* transaction_output]
--   ,   2  : coin
--   , ? 3  : slot_no
--   , ? 4  : certificates
--   , ? 5  : withdrawals
--   , ? 7  : auxiliary_data_hash
--   , ? 8  : slot_no
--   , ? 9  : mint
--   , ? 11 : script_data_hash
--   , ? 13 : nonempty_set<transaction_input>
--   , ? 14 : required_signers
--   , ? 15 : network_id
--   , ? 16 : transaction_output
--   , ? 17 : coin
--   , ? 18 : nonempty_set<transaction_input>
--   , ? 19 : voting_procedures
--   , ? 20 : proposal_procedures
--   , ? 21 : coin
--   , ? 22 : positive_coin
--   }
--
--   So, to mask all inputs, we're looking for keys 0, 13 and 18.
maskTxBodyTerm :: Term -> Term
maskTxBodyTerm = \case
  TMap kvs -> TMap (map maskEntry kvs)
  TMapI kvs -> TMapI (map maskEntry kvs)
  other -> other
  where
    maskEntry (key, value)
      | isInputKey key = (key, maskInputsValue value)
      | otherwise = (key, value)

maskInputsValue :: Term -> Term
maskInputsValue v = case v of
  -- 258 means set in CDDL speak:
  --  set<a0> = #6.258([* a0])/ [* a0]
  --  nonempty_oset<a0> = #6.258([+ a0])/ [+ a0]
  --  nonempty_set<a0> = #6.258([+ a0])/ [+ a0]
  TTagged 258 t -> TTagged 258 (maskInputs t)
  t -> maskInputs t

isInputKey :: Term -> Bool
isInputKey k = termEqualsInt 0 k || termEqualsInt 13 k || termEqualsInt 18 k

maskInputs :: Term -> Term
maskInputs = \case
  TList xs -> TList (map maskInput xs)
  TListI xs -> TListI (map maskInput xs)
  other -> other

maskInput :: Term -> Term
maskInput = \case
  TList (txIdTerm : indexTerm : rest) ->
    TList (maskTxId txIdTerm : maskTxIndex indexTerm : rest)
  TListI (txIdTerm : indexTerm : rest) ->
    TListI (maskTxId txIdTerm : maskTxIndex indexTerm : rest)
  TList (txIdTerm : rest) ->
    TList (maskTxId txIdTerm : rest)
  TListI (txIdTerm : rest) ->
    TListI (maskTxId txIdTerm : rest)
  other -> other

maskTxId :: Term -> Term
maskTxId = \case
  TBytes bs -> TBytes (BS.replicate (BS.length bs) 0)
  TBytesI bs -> TBytesI (BL.replicate (BL.length bs) 0)
  other -> other

maskTxIndex :: Term -> Term
maskTxIndex = \case
  TInt _ ->
    TInt 0
  TInteger _ -> TInteger 0
  other -> other

termEqualsInt :: Integer -> Term -> Bool
termEqualsInt target = \case
  TInt i -> toInteger i == target
  TInteger i -> i == target
  _ -> False
