-- | Module for building Cardano transactions based on high-level intents.
--     This module provides functionality to construct transactions using the Cooked
--     library, interpreting intents that specify spending sources, payment outputs,
--     validity intervals, and change addresses.
module Intent.Example.TxBuilder (
  buildTx,
) where

import Cardano.Api qualified as C (
  AsType (..),
  Quantity (..),
  Value,
  deserialiseAddress,
  lovelaceToValue,
  quantityToLovelace,
  serialiseAddress,
 )
import Cavefish.Services.TxBuilding (ServiceFee (ServiceFee, amount, paidTo))
import Control.Monad (join, unless, when)
import Cooked (
  BalancingPolicy (BalanceWithFirstSignatory),
  MonadBlockChain,
  Payable (Value),
  TxSkel (txSkelSignatories),
  TxSkelOut,
  balanceTxSkel,
  currentSlot,
  emptyTxSkelRedeemer,
  onlyValueOutputsAtSearch,
  receives,
  runUtxoSearch,
  signatoryPubKey,
  txSkelIns,
  txSkelMints,
  txSkelOptBalancingPolicy,
  txSkelOpts,
  txSkelOuts,
  txSkelTemplate,
  txSkelValidityRange,
 )
import Cooked.MockChain.GenerateTx.Body (txSkelToTxBody)
import Data.Map.Strict qualified as Map (fromList)
import Data.Text qualified as Text (unpack)
import Debug.Trace qualified as Debug
import Intent.Example.DSL (
  AdressConwayEra (AdressConwayEra),
  CanonicalIntent (CanonicalIntent, changeTo, maxFee, maxInterval, mustMint, payTo, spendFrom),
 )
import Ledger (Interval, PubKeyHash, Slot, always, cardanoPubKeyHash, interval)
import Ledger.Tx.CardanoAPI (fromCardanoValue)
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (PaymentAddess (unPaymentAddess))
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Cardano.UnsignedTx (UnsignedTx (UnsignedTx))

-- | Build a Cardano transaction based on the provided intent.
--
-- Currently minting is not supported; callers should pass empty 'mustMint'.
buildTx :: MonadBlockChain m => CanonicalIntent -> ServiceFee -> m UnsignedTx
buildTx CanonicalIntent {..} serviceFee = do
  unless (null mustMint) $ fail "buildTx: minting not supported yet"
  when (null spendFrom) $ fail "No source address defined to spend from"
  utxos <- join <$> mapM (runUtxoSearch . onlyValueOutputsAtSearch) spendFrom
  let inputs = Map.fromList [(oref, emptyTxSkelRedeemer) | (oref, _) <- utxos]
  payOuts <- traverse buildPayTo payTo
  feeOuts <- buildServiceFee serviceFee
  validityRange <- buildValidity maxInterval

  let opts =
        (txSkelOpts txSkelTemplate)
          { txSkelOptBalancingPolicy = BalanceWithFirstSignatory
          }
      skelUnbalanced =
        txSkelTemplate
          { txSkelIns = inputs
          , txSkelOuts = payOuts <> feeOuts
          , txSkelMints = mempty -- N.H todo
          -- Balance with 1st signer is default. See [BlancingPolicy](https://github.com/tweag/cooked-validators/blob/main/doc/BALANCING.md#balancing-policy)
          , txSkelSignatories =
              map
                signatoryPubKey
                (maybe spendFrom (: spendFrom) changeTo)
          , txSkelValidityRange = validityRange
          , txSkelOpts = opts
          }

  (skel, fee, collateral) <- balanceTxSkel skelUnbalanced
  case maxFee of
    Just lim
      | fee > lim ->
          fail $
            "buildTx: computed fee "
              <> show fee
              <> " exceeds maxFee "
              <> show lim
    _ -> pure ()
  UnsignedTx <$> txSkelToTxBody skel fee collateral

toPubKeyHash :: MonadFail m => AdressConwayEra -> m PubKeyHash
toPubKeyHash (AdressConwayEra addr) =
  maybe
    (fail $ "buildTx: address is not a payment key hash: " <> Text.unpack (C.serialiseAddress addr))
    pure
    (cardanoPubKeyHash addr)

buildPayTo :: MonadFail m => (C.Value, AdressConwayEra) -> m TxSkelOut
buildPayTo (val, addr) = do
  pkh <- toPubKeyHash addr
  let value = fromCardanoValue val
  pure $ receives pkh (Value value)

buildServiceFee :: MonadFail f => ServiceFee -> f [TxSkelOut]
buildServiceFee ServiceFee {..}
  | amount <= 0 = Debug.trace ("service fee is <= 0" <> show amount) pure []
  | otherwise = do
      addr <-
        maybe
          (fail $ "buildTx: invalid service fee address: " <> Text.unpack (unPaymentAddess paidTo))
          (pure . AdressConwayEra)
          (C.deserialiseAddress (C.AsAddressInEra C.AsConwayEra) (unPaymentAddess paidTo))
      pkh <- toPubKeyHash addr
      let value =
            fromCardanoValue
              (C.lovelaceToValue (C.quantityToLovelace (C.Quantity amount)))
      pure [receives pkh (Value value)]

buildValidity :: MonadBlockChain m => Maybe Slot -> m (Interval Slot)
buildValidity Nothing = pure always
buildValidity (Just slop) = do
  now <- currentSlot
  pure $ interval now (now + slop)
