{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

-- | Module providing an implementation of the transaction building and submission
--    functions using the Cooked mock chain.
--
--    This module defines functions to create a Cooked environment, build transactions,
--    and submit them to the mock chain. It also includes utilities to manage the
--    mock chain state.
module Sp.Emulator (
  mkServerContext,
  buildWithCooked,
  initialMockState,
  producedTotal,
) where

import Cardano.Api (MonadIO (..))
import Cardano.Api qualified as Api
import Control.Concurrent.STM (TVar, readTVarIO)
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
import Core.Api.Config qualified as Cfg
import Core.Api.ServerContext
import Core.Intent
import Core.Observers.Observer (stakeValidatorFromBytes)
import Core.TxBuilder (buildTx)
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Default (def)
import Data.Foldable (traverse_)
import Ledger.Scripts (StakeValidator (getStakeValidator))
import Ledger.Tx (
  pattern CardanoEmulatorEraTx,
 )
import Plutus.Script.Utils.Address qualified as ScriptAddr
import Plutus.Script.Utils.Scripts (Language (PlutusV2), Versioned (Versioned))
import Servant (
  err422,
  err500,
  errBody,
  throwError,
 )
import WBPS.Commitment qualified as WBPS
import WBPS.Core.FileScheme (FileScheme)
import WBPS.Registration (RegistrationFailed (AccountAlreadyRegistered), withFileSchemeIO)
import WBPS.Registration qualified as WBPS

mkServerContext ::
  TVar MockChainState ->
  FileScheme ->
  Cfg.Config ->
  ServerContext
mkServerContext
  mockChainState
  wbpsScheme
  config =
    env
    where
      env =
        ServerContext
          { txBuildingServices =
              TxBuildingServices
                { serviceFeeAmount = Cfg.amount (Cfg.serviceProviderFee config)
                , build = buildWithCooked mockChainState env
                , submit = submitWithCooked mockChainState env
                }
          , wbpsServices =
              WBPSServices
                { register = \userWalletPublicKey ->
                    liftIO (withFileSchemeIO wbpsScheme (WBPS.register userWalletPublicKey))
                      >>= \case
                        Left [AccountAlreadyRegistered _] -> throwError err422 {errBody = BL8.pack "Account Already Registered"}
                        Left e -> throwError err500 {errBody = BL8.pack ("Unexpected event" ++ show e)}
                        Right x -> pure x
                , createSession = \userWalletPublicKey tx ->
                    liftIO (WBPS.withFileSchemeIO wbpsScheme (WBPS.createSession userWalletPublicKey tx))
                      >>= \case
                        (Left e) -> throwError err500 {errBody = BL8.pack ("Unexpected event" ++ show e)}
                        (Right x) -> pure x
                , loadAccount = \userWalletPublicKey ->
                    liftIO (WBPS.withFileSchemeIO wbpsScheme (WBPS.loadAccount userWalletPublicKey))
                      >>= \case
                        (Left e) -> throwError err500 {errBody = BL8.pack ("Unexpected event" ++ show e)}
                        Right x -> pure x
                , loadAccounts =
                    liftIO (withFileSchemeIO wbpsScheme WBPS.loadAccounts)
                      >>= \case
                        (Left e) -> throwError err500 {errBody = BL8.pack ("Unexpected event" ++ show e)}
                        Right x -> pure x
                }
          }

-- | Build a transaction using the Cooked mock chain.
buildWithCooked ::
  MonadIO m =>
  TVar MockChainState ->
  ServerContext ->
  IntentDSL ->
  m TxUnsigned
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
          pure . TxUnsigned $ tx

-- | Submit a transaction to the mock chain by updating the mock chain state.
submitWithCooked ::
  MonadIO m =>
  TVar MockChainState ->
  ServerContext ->
  Api.Tx Api.ConwayEra ->
  m ()
submitWithCooked _ _env _tx = do
  -- atomically $ writeTVar mockState newState
  pure ()

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
