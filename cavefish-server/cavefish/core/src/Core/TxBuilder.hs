{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- | Module for building Cardano transactions based on high-level intents.
--     This module provides functionality to construct transactions using the Cooked
--     library, interpreting intents that specify spending sources, payment outputs,
--     validity intervals, and change addresses.
module Core.TxBuilder (
  buildTx,
) where

import Control.Monad (join)
import Cooked
import Cooked.MockChain.GenerateTx.Body (txSkelToTxBody)
import Core.Api.ServerContext (
  ServiceFee,
 )
import Core.Intent

-- | Build a Cardano transaction based on the provided intent
buildTx :: MonadBlockChain m => CanonicalIntent -> ServiceFee -> m TxUnsigned
buildTx CanonicalIntent {..} serviceFeeAmount = do
  inputs <- join $ mapM (runUtxoSearch . onlyValueOutputsAtSearch) spendFrom

  let template :: TxSkel = TxSkel {}
  (skel, fee, collateral) <- balanceTxSkel template
  TxUnsigned <$> txSkelToTxBody skel fee collateral
