module Core.TxAbs where

import Cardano.Api
import Cardano.Api qualified as Api
import Cardano.Api.Ledger (VKey (..))
import Cardano.Api.Shelley (Tx (..))
import Cardano.Crypto.DSIGN.Class (rawSerialiseVerKeyDSIGN)
import Cardano.Ledger.Alonzo.Tx (AlonzoTx (..))
import Cardano.Ledger.Alonzo.TxWits (AlonzoTxWits (..))
import Cardano.Ledger.Keys.WitVKey qualified as LedgerWit
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Ledger (
  CardanoTx (CardanoEmulatorEraTx),
  Coin (Coin),
  Interval,
  PubKey (..),
  Slot,
  cardanoPubKeyHash,
  getCardanoTxFee,
  getCardanoTxMint,
  getCardanoTxOutputs,
  getCardanoTxValidityRange,
  pubKeyHash,
 )
import Ledger.Tx qualified as LedgerTx
import PlutusLedgerApi.V1.Bytes

data TxAbs era = TxAbs
  { outputs :: [TxOut CtxTx era]
  , validityInterval :: Interval Slot
  , absMint :: Value
  , absFee :: Integer
  , sigKeys :: Set.Set PubKey
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

cardanoTxToTxAbs :: CardanoTx -> TxAbs Api.ConwayEra
cardanoTxToTxAbs cardanoTx@(CardanoEmulatorEraTx tx) =
  let signerKeys = extractSignerKeys tx
      txOutputs = [out | LedgerTx.TxOut out <- getCardanoTxOutputs cardanoTx]
   in TxAbs
        { outputs = maskChangeOutput signerKeys txOutputs
        , validityInterval = getCardanoTxValidityRange cardanoTx
        , absMint = getCardanoTxMint cardanoTx
        , absFee = let Coin fee = getCardanoTxFee cardanoTx in fee
        , sigKeys = signerKeys
        }

extractSignerKeys :: Api.Tx Api.ConwayEra -> Set.Set PubKey
extractSignerKeys (ShelleyTx _ AlonzoTx{wits = AlonzoTxWits{txwitsVKey}}) =
  Set.fromList (map witnessToPubKey (Set.toList txwitsVKey))
 where
  witnessToPubKey (LedgerWit.WitVKey vkey _) =
    let rawKey = rawSerialiseVerKeyDSIGN (unVKey vkey)
     in PubKey (fromBytes rawKey)

-- This is in accordance with the paper's statement (section 23:9):
-- The value of the first output is zeroed out to allow for
-- any leftover input funds to be returned to the LC’s change address.
maskChangeOutput ::
  Set.Set PubKey ->
  [TxOut CtxTx Api.ConwayEra] ->
  [TxOut CtxTx Api.ConwayEra]
maskChangeOutput signerKeys =
  go
 where
  signerHashes = Set.fromList (map pubKeyHash (Set.toList signerKeys))

  go :: [TxOut CtxTx Api.ConwayEra] -> [TxOut CtxTx Api.ConwayEra]
  go [] = []
  go (out@(TxOut addr value datum refScript) : rest)
    | Just pkh <- cardanoPubKeyHash addr
    , Set.member pkh signerHashes
    , valuePositive (Api.txOutValueToValue value) =
        -- Mask the first positive output controlled by a signer; this is the
        -- change output that the paper requires us to hide.
        zeroTxOutValue (TxOut addr value datum refScript) : go rest
    | otherwise =
        out : go rest

valuePositive :: Api.Value -> Bool
valuePositive = any (\(_, Api.Quantity q) -> q > 0) . Api.valueToList

zeroTxOutValue :: TxOut CtxTx Api.ConwayEra -> TxOut CtxTx Api.ConwayEra
zeroTxOutValue (TxOut addr value datum refScript) =
  TxOut addr (zeroValue value) datum refScript
 where
  zeroValue :: TxOutValue era -> TxOutValue era
  zeroValue (TxOutValueShelleyBased era ledgerValue) =
    TxOutValueShelleyBased era $
      case era of
        Api.ShelleyBasedEraMary -> Api.toLedgerValue Api.MaryEraOnwardsMary mempty
        Api.ShelleyBasedEraAlonzo -> Api.toLedgerValue Api.MaryEraOnwardsAlonzo mempty
        Api.ShelleyBasedEraBabbage -> Api.toLedgerValue Api.MaryEraOnwardsBabbage mempty
        Api.ShelleyBasedEraConway -> Api.toLedgerValue Api.MaryEraOnwardsConway mempty
        Api.ShelleyBasedEraShelley -> ledgerValue
        Api.ShelleyBasedEraAllegra -> ledgerValue
  zeroValue other = other
