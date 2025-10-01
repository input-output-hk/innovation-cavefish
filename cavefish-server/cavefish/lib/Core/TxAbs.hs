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
import Ledger (CardanoTx (CardanoEmulatorEraTx), Coin (Coin), Interval, PubKey (..), Slot, getCardanoTxFee, getCardanoTxMint, getCardanoTxOutputs, getCardanoTxValidityRange)
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
  TxAbs
    { outputs = [out | LedgerTx.TxOut out <- getCardanoTxOutputs cardanoTx]
    , validityInterval = getCardanoTxValidityRange cardanoTx
    , absMint = getCardanoTxMint cardanoTx
    , absFee = let Coin fee = getCardanoTxFee cardanoTx in fee
    , sigKeys = extractSignerKeys tx
    }

extractSignerKeys :: Api.Tx Api.ConwayEra -> Set.Set PubKey
extractSignerKeys (ShelleyTx _ AlonzoTx{wits = AlonzoTxWits{txwitsVKey}}) =
  Set.fromList (map witnessToPubKey (Set.toList txwitsVKey))
 where
  witnessToPubKey (LedgerWit.WitVKey vkey _) =
    let rawKey = rawSerialiseVerKeyDSIGN (unVKey vkey)
     in PubKey (fromBytes rawKey)
