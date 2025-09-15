{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use list comprehension" #-}
module Sp.TxBuilder where

import Cardano.Api qualified as Api
import Control.Monad (foldM)
import Cooked
import Cooked.Skeleton.Payable qualified as Payable
import Core.Intent (Intent (..))
import Data.Text (Text)
import Data.Text qualified as T
import Ledger.Scripts (Script, StakeValidator, ToVersioned)
import Ledger.Tx (CardanoTx)
import Plutus.Script.Utils.Value qualified as PSV

-- TODO WG: Merge with `Env`
data Config = Config
  { spWallet :: Wallet
  , fundingWallet :: Wallet
  , observerStakeVal :: StakeValidator
  , observerRedeemer :: TxSkelRedeemer
  , resolveWallet :: Api.AddressInEra Api.ConwayEra -> Maybe Wallet
  , spFee :: Integer
  }

buildTx ::
  (MonadBlockChain m, ToVersioned Script StakeValidator) =>
  Intent Api.ConwayEra ->
  Config ->
  m CardanoTx
buildTx intent Config{..} = do
  payPlan <- either (fail . T.unpack) pure (collectPayments intent)

  let outsPay :: [TxSkelOut]
      outsPay = map (\(w, amt) -> w `receives` Payable.Value (PSV.ada amt)) payPlan

      outsFee :: [TxSkelOut]
      outsFee =
        if spFee > 0
          then [spWallet `receives` Payable.Value (PSV.ada spFee)]
          else []

      outs :: [TxSkelOut]
      outs = outsPay ++ outsFee

      wdrl = scriptWithdrawal observerStakeVal observerRedeemer 0

      skel =
        txSkelTemplate
          { txSkelOuts = outs
          , txSkelSigners = [fundingWallet]
          , txSkelWithdrawals = wdrl
          }

  validateTxSkel skel
 where
  collectPayments ::
    Intent Api.ConwayEra ->
    Either Text [(Wallet, Integer)]
  collectPayments = \case
    AndExps is -> concat <$> traverse collectPayments is
    PayTo v addr ->
      case resolveWallet addr of
        Nothing -> Left "TxBuilder: address not recognised (resolveWallet failed)"
        Just w -> do
          lov <- toLovelace v
          pure [(w, lov)]
    MaxFee _ -> undefined
    MaxInterval _ -> undefined
    ChangeTo _ -> undefined
    MustMint _ -> undefined
    SpendFrom _ -> undefined

  toLovelace :: Api.Value -> Either Text Integer
  toLovelace =
    foldM step 0 . Api.valueToList
   where
    step :: Integer -> (Api.AssetId, Api.Quantity) -> Either Text Integer
    step acc (aid, Api.Quantity q) =
      case aid of
        Api.AdaAssetId -> Right (acc + q)
        Api.AssetId{} -> Left "TxBuilder: unsupported (WIP)"