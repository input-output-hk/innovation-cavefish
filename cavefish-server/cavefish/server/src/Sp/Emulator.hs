{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- | Module providing an implementation of the transaction building and submission
--    functions using the Cooked mock chain.
--
--    This module defines functions to create a Cooked environment, build transactions,
--    and submit them to the mock chain. It also includes utilities to manage the
--    mock chain state.
module Sp.Emulator (
  mkCookedEnv,
  buildWithCooked,
  initialMockState,
  producedTotal,
) where

import Cardano.Api (MonadIO (..))
import Cardano.Api qualified as Api
import Control.Concurrent.STM (TVar, atomically, readTVarIO, writeTVar)
import Control.Monad.Identity (runIdentity)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State.Strict (runStateT)
import Control.Monad.Trans.Writer (WriterT (runWriterT))
import Cooked (MockChain, MockChainError, MockChainT (unMockChain))
import Cooked.MockChain (registerStakingCred)
import Cooked.MockChain.MockChainState (
  MockChainState,
  mockChainState0,
 )
import Core.Api.AppContext (
  Env (..),
  defaultWalletResolver,
 )
import Core.Api.Config qualified as Cfg
import Core.Api.State (CompleteStore, PendingStore)
import Core.Intent
import Core.Observers.Observer (stakeValidatorFromBytes)
import Core.TxBuilder (buildTx)
import Data.Default (def)
import Data.Foldable (traverse_)
import Data.Text (Text)
import Ledger.Scripts (StakeValidator (getStakeValidator))
import Ledger.Tx (
  pattern CardanoEmulatorEraTx,
 )
import Plutus.Script.Utils.Address qualified as ScriptAddr
import Plutus.Script.Utils.Scripts (Language (PlutusV2), Versioned (Versioned))
import WBPS.Core.FileScheme (FileScheme)

-- | Create a Cooked environment for the Cavefish server using the provided
-- parameters.
mkCookedEnv ::
  TVar MockChainState ->
  PendingStore ->
  CompleteStore ->
  FileScheme ->
  Cfg.Config ->
  Env
mkCookedEnv
  mockState
  pendingStore
  completeStore
  wbpsSchemeValue
  config =
    env
    where
      env =
        Env
          { pending = pendingStore
          , complete = completeStore
          , ttl = fromInteger $ Cfg.seconds $ Cfg.transactionExpiry config
          , resolveWallet = defaultWalletResolver
          , spFee = Cfg.amount (Cfg.serviceProviderFee config)
          , wbpsScheme = wbpsSchemeValue
          , build = buildWithCooked mockState env
          , submit = submitWithCooked mockState env
          }

-- | Build a transaction using the Cooked mock chain.
buildWithCooked ::
  MonadIO m =>
  TVar MockChainState ->
  Env ->
  IntentDSL ->
  m BuildTxResult
buildWithCooked mockState env intentDSL = do
  case toCanonicalIntent intentDSL of
    Left err -> liftIO $ fail ("buildTx failed: " <> show err)
    Right canonicalIntent -> do
      st0 <- liftIO $ readTVarIO mockState
      let (result, st1) =
            runMockChainPure st0 $ do
              let stakeValidator = fmap stakeValidatorFromBytes Nothing
                  cred =
                    fmap
                      ( \sv ->
                          ScriptAddr.toCredential
                            (Versioned (getStakeValidator sv) PlutusV2)
                      )
                      stakeValidator
              traverse_ (\c -> registerStakingCred c 0 0) cred
              buildTx canonicalIntent Nothing env
      case result of
        Left err -> liftIO $ fail ("buildTx failed: " <> show err)
        Right (CardanoEmulatorEraTx tx) -> do
          -- let rawOutputs = [out | LedgerTx.TxOut out <- getCardanoTxOutputs cardanoTx]
          --     -- txAbs = cardanoTxToTxAbs cardanoTx
          --     producedMasked = producedTotal (outputs txAbs)
          --     producedRaw = producedTotal rawOutputs
          --     -- The masked TxAbs zeroes the change output, so we recover the hidden
          --     -- amount by subtracting masked outputs from their raw counterparts.
          --     changeDelta = producedRaw <> Api.negateValue producedMasked
          pure
            BuildTxResult
              { tx
              , -- , changeDelta
                -- , txAbs
                mockState = st1
              }

-- | Submit a transaction to the mock chain by updating the mock chain state.
submitWithCooked ::
  TVar MockChainState ->
  Env ->
  Api.Tx Api.ConwayEra ->
  MockChainState ->
  IO (Either Text ())
submitWithCooked mockState _env _tx newState = do
  atomically $ writeTVar mockState newState
  pure (Right ())

-- | Run a 'MockChain' action purely, returning the result and the new state.
runMockChainPure ::
  MockChainState ->
  MockChain a ->
  (Either MockChainError a, MockChainState)
runMockChainPure st action =
  let ((result, newState), _) =
        runIdentity $
          runWriterT $
            runStateT (runExceptT (unMockChain action)) st
   in (result, newState)

-- | Calculate the total value produced by a list of transaction outputs.
producedTotal ::
  [Api.TxOut Api.CtxTx Api.ConwayEra] ->
  ChangeDelta
producedTotal outs =
  mconcat
    [ Api.txOutValueToValue val
    | Api.TxOut _ val _ _ <- outs
    ]

-- | The initial mock chain state used by the Cavefish server.
initialMockState :: MockChainState
initialMockState =
  case runMockChainPure def mockChainState0 of
    (Left err, _) -> error ("failed to initialise mock chain state: " <> show err)
    (Right st, _) -> st
