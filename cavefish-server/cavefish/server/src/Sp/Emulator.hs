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
) where

import Cardano.Api qualified as Api
import Control.Concurrent.STM (TVar, atomically, readTVarIO, writeTVar)
import Control.Monad.Identity (runIdentity)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State.Strict (runStateT)
import Control.Monad.Trans.Writer (WriterT (runWriterT))
import Cooked (MockChain, MockChainError, MockChainT (unMockChain), Wallet)
import Cooked.MockChain (registerStakingCred)
import Cooked.MockChain.MockChainState (
  MockChainState,
  mockChainState0,
 )
import Core.Api.AppContext (
  Env (
    Env,
    build,
    clientRegistration,
    complete,
    pending,
    pkePublic,
    pkeSecret,
    resolveWallet,
    spFee,
    spSk,
    spWallet,
    submit,
    ttl,
    wbpsScheme
  ),
  defaultWalletResolver,
 )
import Core.Api.Config qualified as Cfg
import Core.Api.State (ClientRegistrationStore, CompleteStore, PendingStore)
import Core.Intent (
  BuildTxResult (BuildTxResult, changeDelta, mockState, tx, txAbs),
  ChangeDelta,
  Intent,
 )
import Core.Observers.Observer (stakeValidatorFromBytes)
import Core.Pke (PkeSecretKey, toPublicKey)
import Core.TxAbs (TxAbs (outputs), cardanoTxToTxAbs)
import Core.TxBuilder (buildTx)
import Crypto.PubKey.Ed25519 (SecretKey)
import Data.ByteString (ByteString)
import Data.Default (def)
import Data.Foldable (traverse_)
import Data.Text (Text)
import Ledger (
  getCardanoTxOutputs,
 )
import Ledger.Scripts (StakeValidator (getStakeValidator))
import Ledger.Tx (
  pattern CardanoEmulatorEraTx,
 )
import Ledger.Tx qualified as LedgerTx
import Plutus.Script.Utils.Address qualified as ScriptAddr
import Plutus.Script.Utils.Scripts (Language (PlutusV2), Versioned (Versioned))
import WBPS.Core.FileScheme (FileScheme)

-- | Create a Cooked environment for the Cavefish server using the provided
-- parameters.
mkCookedEnv ::
  TVar MockChainState ->
  PendingStore ->
  CompleteStore ->
  SecretKey ->
  PkeSecretKey ->
  Wallet ->
  FileScheme ->
  Cfg.Config ->
  Env
mkCookedEnv
  mockState
  pendingStore
  completeStore
  clientRegStore
  spSk
  pkeSk
  spWallet
  wbpsSchemeValue
  config =
    env
    where
      pkePk = toPublicKey pkeSk
      env =
        Env
          { spSk
          , pending = pendingStore
          , complete = completeStore
          , clientRegistration = clientRegStore
          , ttl = fromInteger $ Cfg.seconds $ Cfg.transactionExpiry config
          , spWallet
          , resolveWallet = defaultWalletResolver
          , spFee = Cfg.amount (Cfg.serviceProviderFee config)
          , pkeSecret = pkeSk
          , pkePublic = pkePk
          , wbpsScheme = wbpsSchemeValue
          , build = buildWithCooked mockState env
          , submit = submitWithCooked mockState env
          }

-- | Build a transaction using the Cooked mock chain.
buildWithCooked ::
  TVar MockChainState ->
  Env ->
  Intent ->
  Maybe ByteString ->
  IO BuildTxResult
buildWithCooked mockState env intent observerBytes = do
  st0 <- readTVarIO mockState
  let (result, st1) =
        runMockChainPure st0 $ do
          let stakeValidator = fmap stakeValidatorFromBytes observerBytes
              cred =
                fmap
                  ( \sv ->
                      ScriptAddr.toCredential
                        (Versioned (getStakeValidator sv) PlutusV2)
                  )
                  stakeValidator
          traverse_ (\c -> registerStakingCred c 0 0) cred
          buildTx intent observerBytes env
  case result of
    Left err -> fail ("buildTx failed: " <> show err)
    Right cardanoTx@(CardanoEmulatorEraTx tx) -> do
      let rawOutputs = [out | LedgerTx.TxOut out <- getCardanoTxOutputs cardanoTx]
          txAbs = cardanoTxToTxAbs cardanoTx
          producedMasked = producedTotal (outputs txAbs)
          producedRaw = producedTotal rawOutputs
          -- The masked TxAbs zeroes the change output, so we recover the hidden
          -- amount by subtracting masked outputs from their raw counterparts.
          changeDelta = producedRaw <> Api.negateValue producedMasked
      pure
        BuildTxResult
          { tx
          , changeDelta
          , txAbs
          , mockState = st1
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
