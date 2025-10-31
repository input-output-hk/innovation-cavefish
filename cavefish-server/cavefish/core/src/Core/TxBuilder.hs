-- | Module for building Cardano transactions based on high-level intents.
--     This module provides functionality to construct transactions using the Cooked
--     library, interpreting intents that specify spending sources, payment outputs,
--     validity intervals, and change addresses.
module Core.TxBuilder where

import Cardano.Api qualified as Api
import Control.Monad (foldM, unless, when)
import Cooked (
  BalanceOutputPolicy (DontAdjustExistingOutput),
  BalancingPolicy (BalanceWith),
  MonadBlockChain (validateTxSkel),
  TxOpts (txOptBalanceOutputPolicy, txOptBalancingPolicy),
  TxSkel (
    txSkelOpts,
    txSkelOuts,
    txSkelSigners,
    txSkelValidityRange,
    txSkelWithdrawals
  ),
  TxSkelOut,
  Wallet,
  currentSlot,
  emptyTxSkelRedeemer,
  receives,
  scriptWithdrawal,
  txSkelTemplate,
 )
import Cooked.Skeleton.Payable qualified as Payable
import Core.Api.AppContext (Env (Env, resolveWallet, spFee, spWallet))
import Core.Intent (Intent (irChangeTo, irMaxInterval, irMustMint, irPayTo, irSpendFrom), source)
import Core.Observers.Observer (stakeValidatorFromBytes)
import Data.ByteString (ByteString)
import Data.List (nub)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Exts (IsList (toList))
import Ledger.Tx (CardanoTx)
import Plutus.Script.Utils.Value qualified as PSV
import PlutusLedgerApi.V1.Interval qualified as Interval

-- | Build a Cardano transaction based on the provided intent and observer
buildTx :: MonadBlockChain m => Intent -> ByteString -> Env -> m CardanoTx
buildTx intent observerBytes env@Env {..} = do
  let stakeValidator = stakeValidatorFromBytes observerBytes
      skel0 = base stakeValidator
  signerWallets <-
    nub
      <$> mapM
        ( \addr -> case getWallet addr of
            Left err -> fail (T.unpack err)
            Right wallet -> pure wallet
        )
        (source <$> intent.irSpendFrom)
  let skelWithSigners =
        case signerWallets of
          [] -> skel0
          (w : ws) ->
            skel0
              { txSkelSigners = w : ws
              , txSkelOpts = (txSkelOpts skel0) {txOptBalancingPolicy = BalanceWith w}
              }
  unless (null (irPayTo intent) || not (null signerWallets)) $
    fail "TxBuilder: pay outputs require at least one funding source"
  skel1 <- foldM addPay skelWithSigners (irPayTo intent)
  -- TODO: This is non-trivial to implement, so I'm leaving it for now.
  case irMustMint intent of
    [] -> pure ()
    _ -> fail "unsupported intent: must mint"
  skel2 <- case irMaxInterval intent of
    Nothing -> pure skel1
    Just maxInterval -> do
      start <- currentSlot
      -- TODO WG: Realism
      let end = start + maxInterval
      when (end < start) $ fail "TxBuilder: max interval must be non-negative"
      pure skel1 {txSkelValidityRange = Interval.interval start end}
  skel3 <- case irChangeTo intent of
    Nothing -> pure skel2
    Just addr -> do
      wallet <- either (fail . T.unpack) pure (getWallet addr)
      let opts =
            (txSkelOpts skel2)
              { -- TODO WG: Realism (maybe?)
                txOptBalancingPolicy = BalanceWith wallet
              , txOptBalanceOutputPolicy = DontAdjustExistingOutput
              }
      pure skel2 {txSkelOpts = opts}
  -- MaxFee is a post-condition only; no builder action required
  validateTxSkel skel3
  where
    base stakeValidator' =
      txSkelTemplate
        { txSkelSigners = []
        , txSkelWithdrawals = scriptWithdrawal stakeValidator' emptyTxSkelRedeemer 0
        , txSkelOuts = spFeeOutputs
        }

    spFeeOutputs =
      [spWallet `receives` Payable.Value (PSV.ada spFee) | spFee > 0]

    addPay :: MonadBlockChain m => TxSkel -> (Api.Value, Api.AddressInEra Api.ConwayEra) -> m TxSkel
    addPay skel (v, addr) = do
      outs <- either (fail . T.unpack) pure (mkPayTo v addr env)
      pure skel {txSkelOuts = txSkelOuts skel ++ outs}
    getWallet ::
      Api.AddressInEra Api.ConwayEra ->
      Either Text Wallet
    getWallet addr' =
      case resolveWallet addr' of
        Nothing -> Left "TxBuilder: address not recognised (resolveWallet failed)"
        Just w -> pure w

mkPayTo ::
  Api.Value ->
  Api.AddressInEra Api.ConwayEra ->
  Env ->
  Either Text [TxSkelOut]
mkPayTo value addr Env {..} = do
  payee <- case resolveWallet addr of
    Nothing -> Left "TxBuilder: address not recognised (resolveWallet failed)"
    Just w -> Right w
  lov <- toLovelace value
  pure [payee `receives` Payable.Value (PSV.ada lov)]

toLovelace :: Api.Value -> Either Text Integer
toLovelace =
  foldM step 0 . toList
  where
    step :: Integer -> (Api.AssetId, Api.Quantity) -> Either Text Integer
    step acc (aid, Api.Quantity q) =
      case aid of
        Api.AdaAssetId -> Right (acc + q)
        -- TODO WG (but maybe we don't care for the prototype)
        Api.AssetId {} -> Left "TxBuilder: unsupported (WIP)"
