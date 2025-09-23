module Sp.TxBuilder where

import Cardano.Api qualified as Api
import Control.Monad (foldM, when)
import Cooked
import Cooked.Skeleton.Payable qualified as Payable
import Core.Intent (Env (..), Intent (..))
import Data.ByteString (ByteString)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Ledger.Tx (CardanoTx)
import Observers.Observer (stakeValidatorFromBytes)
import Plutus.Script.Utils.Value qualified as PSV
import PlutusLedgerApi.V1.Interval qualified as Interval

buildTx :: (MonadBlockChain m) => Intent -> ByteString -> Env -> m CardanoTx
buildTx intent observerBytes env@Env{..} = do
  let stakeValidator = stakeValidatorFromBytes observerBytes
      skel0 = base stakeValidator
  skel1 <- foldM addPay skel0 (irPayTo intent)
  skel2 <- foldM addSpend skel1 (irSpendFrom intent)
  -- TODO: This is non-trivial to implement, so I'm leaving it for now.
  case irMustMint intent of
    [] -> pure ()
    _ -> fail "unsupported intent: must mint"
  skel3 <- case irMaxInterval intent of
    Nothing -> pure skel2
    Just maxInterval -> do
      start <- currentSlot
      -- TODO WG: Realism
      let end = start + maxInterval
      when (end < start) $ fail "TxBuilder: max interval must be non-negative"
      pure skel2{txSkelValidityRange = Interval.interval start end}
  skel4 <- case irChangeTo intent of
    Nothing -> pure skel3
    Just addr -> do
      wallet <- either (fail . T.unpack) pure (getWallet addr)
      let opts =
            (txSkelOpts skel3)
              { -- TODO WG: Realism (maybe?)
                txOptBalancingPolicy = BalanceWith wallet
              , txOptBalanceOutputPolicy = DontAdjustExistingOutput
              }
      pure skel3{txSkelOpts = opts}
  -- MaxFee is a post-condition only; no builder action required
  validateTxSkel skel4
 where
  base stakeValidator' =
    txSkelTemplate
      { txSkelSigners = []
      , txSkelWithdrawals = scriptWithdrawal stakeValidator' emptyTxSkelRedeemer 0
      , txSkelOuts = spFeeOutputs
      }

  spFeeOutputs =
    if spFee > 0
      then [spWallet `receives` Payable.Value (PSV.ada spFee)]
      else []

  addPay :: (MonadBlockChain m) => TxSkel -> (Api.Value, Api.AddressInEra Api.ConwayEra) -> m TxSkel
  addPay skel (v, addr) = do
    outs <- either (fail . T.unpack) pure (mkPayTo v addr env)
    pure skel{txSkelOuts = txSkelOuts skel ++ outs}

  addSpend :: (MonadBlockChain m) => TxSkel -> Api.AddressInEra Api.ConwayEra -> m TxSkel
  addSpend skel addr = do
    wallet <- either (fail . T.unpack) pure (getWallet addr)
    utxos <- utxosAt wallet
    -- TODO WG: Realism - Proper coin selection
    (oref, _) <- maybe (fail "TxBuilder: no UTxO available for wallet") pure (listToMaybe utxos)
    let ins' = Map.insert oref emptyTxSkelRedeemer (txSkelIns skel)
        signers' =
          if wallet `elem` txSkelSigners skel
            then txSkelSigners skel
            else txSkelSigners skel ++ [wallet]
    pure skel{txSkelIns = ins', txSkelSigners = signers'}
  getWallet ::
    Api.AddressInEra Api.ConwayEra ->
    Either Text Wallet
  getWallet addr' =
    -- TODO WG: Realism: `resolveWallet` is just a stub
    case resolveWallet addr' of
      Nothing -> Left "TxBuilder: address not recognised (lookupWallet failed)"
      Just w -> pure w

mkPayTo ::
  Api.Value ->
  Api.AddressInEra Api.ConwayEra ->
  Env ->
  Either Text [TxSkelOut]
mkPayTo value addr Env{..} = do
  payPlan <- collectPayments
  let outsPay = [w `receives` Payable.Value (PSV.ada lov) | (w, lov) <- payPlan]
  pure outsPay
 where
  collectPayments :: Either Text [(Wallet, Integer)]
  collectPayments =
    -- TODO WG: Realism - `resolveWallet` is just a stub, see if you can do it "realistically" while still using cooked
    case resolveWallet addr of
      Nothing -> Left "TxBuilder: address not recognised (resolveWallet failed)"
      Just w -> do
        lov <- toLovelace value
        pure [(w, lov)]

toLovelace :: Api.Value -> Either Text Integer
toLovelace =
  foldM step 0 . Api.valueToList
 where
  step :: Integer -> (Api.AssetId, Api.Quantity) -> Either Text Integer
  step acc (aid, Api.Quantity q) =
    case aid of
      Api.AdaAssetId -> Right (acc + q)
      -- TODO WG (but maybe we don't care for the prototype)
      Api.AssetId{} -> Left "TxBuilder: unsupported (WIP)"
