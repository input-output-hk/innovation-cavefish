{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- | Module for building Cardano transactions based on high-level intents.
--     This module provides functionality to construct transactions using the Cooked
--     library, interpreting intents that specify spending sources, payment outputs,
--     validity intervals, and change addresses.
module Intent.Example.TxBuilder (
  buildTx,
) where

import Cardano.Api (AddressInEra, ConwayEra)
import Cardano.Api qualified as C
import Cavefish.Services.TxBuilding (ServiceFee (..))
import Control.Monad (join, unless, when)
import Cooked
import Cooked.MockChain.GenerateTx.Body (txSkelToTxBody)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Data.Maybe (maybeToList)
import Data.Text qualified as Text
import Intent.Example.DSL
import Ledger (Interval, PubKeyHash, Slot, cardanoPubKeyHash, interval)
import Ledger qualified
import Ledger.CardanoWallet qualified as CW
import Ledger.Tx.CardanoAPI (fromCardanoValue)
import WBPS.Core.Keys.Ed25519 (PaymentAddess (unPaymentAddess))

-- | Build a Cardano transaction based on the provided intent.
--
-- Currently minting is not supported; callers should pass empty 'mustMint'.
buildTx :: MonadBlockChain m => CanonicalIntent -> ServiceFee -> m TxUnsigned
buildTx CanonicalIntent {..} serviceFee = do
  unless (null mustMint) $ fail "buildTx: minting not supported yet"
  spendWallets <- traverse resolveWallet spendFrom
  changeWallet <- traverse resolveWallet changeTo
  let signers = nub (spendWallets <> maybeToList changeWallet)
  when (null signers) $
    fail "buildTx: no signer wallets resolved from spendFrom/changeTo"

  inputs <- join <$> mapM (runUtxoSearch . onlyValueOutputsAtSearch) spendWallets
  payOuts <- traverse buildPayTo payTo
  feeOuts <- buildServiceFee serviceFee
  validityRange <- buildValidity maxInterval

  let opts =
        (txSkelOpts txSkelTemplate)
          { txSkelOptBalancingPolicy = maybe BalanceWithFirstSigner BalanceWith changeWallet
          }
      skelUnbalanced =
        txSkelTemplate
          { txSkelIns = Map.fromList ((,emptyTxSkelRedeemer) . fst <$> inputs)
          , txSkelOuts = payOuts <> feeOuts
          , txSkelMints = mempty -- N.H todo
          , txSkelSigners = signers
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
  TxUnsigned <$> txSkelToTxBody skel fee collateral

resolveWallet :: MonadFail m => AdressConwayEra -> m Wallet
resolveWallet (AdressConwayEra addr) =
  maybe
    (fail $ "buildTx: address not in known wallets: " <> Text.unpack (C.serialiseAddress addr))
    pure
    (defaultWalletResolver addr)

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
  | amount <= 0 = pure []
  | otherwise = do
      addr <-
        maybe
          (fail $ "buildTx: invalid service fee address: " <> Text.unpack (unPaymentAddess paymentAddress))
          (pure . AdressConwayEra)
          (C.deserialiseAddress (C.AsAddressInEra C.AsConwayEra) (unPaymentAddess paymentAddress))
      pkh <- toPubKeyHash addr
      let value =
            fromCardanoValue
              (C.lovelaceToValue (C.quantityToLovelace (C.Quantity amount)))
      pure [receives pkh (Value value)]

buildValidity :: MonadBlockChain m => Maybe Slot -> m (Interval Slot)
buildValidity Nothing = pure Ledger.always
buildValidity (Just slop) = do
  now <- currentSlot
  pure $ interval now (now + slop)

resolveWalletFromList ::
  [Wallet] ->
  AddressInEra ConwayEra ->
  Maybe Wallet
resolveWalletFromList wallets =
  let walletIndex =
        Map.fromList
          [ (Ledger.unPaymentPubKeyHash (CW.paymentPubKeyHash w), w)
          | w <- wallets
          ]
   in \addr -> do
        pkh <- Ledger.cardanoPubKeyHash addr
        Map.lookup pkh walletIndex

defaultWalletResolver :: AddressInEra ConwayEra -> Maybe Wallet
defaultWalletResolver = resolveWalletFromList knownWallets
