{-# LANGUAGE PatternSynonyms #-}

-- | Module providing an implementation of the transaction building and submission
--    functions using the Cooked mock chain.
--
--    This module defines functions to create a Cooked environment, build transactions,
--    and submit them to the mock chain. It also includes utilities to manage the
--    mock chain state.
module Sp.Emulator where

import Cardano.Api qualified as Api
import Control.Concurrent.STM (TVar, atomically, readTVarIO, writeTVar)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State.Strict (runStateT)
import Control.Monad.Trans.Writer (WriterT (..))
import Cooked (MockChain, MockChainError, MockChainT (..), Wallet)
import Cooked.MockChain (registerStakingCred)
import Cooked.MockChain.MockChainState (
  MockChainState (..),
  mockChainState0,
 )
import Core.Intent (
  BuildTxResult (..),
  ChangeDelta,
  Intent,
 )
import Core.Pke (PkeSecretKey, toPublicKey)
import Core.TxAbs (TxAbs (..), cardanoTxToTxAbs)
import Crypto.PubKey.Ed25519 (SecretKey)
import Data.ByteString (ByteString)
import Data.Default (def)
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import Ledger (
  getCardanoTxOutputs,
 )
import Ledger.Scripts (StakeValidator (getStakeValidator))
import Ledger.Tx (
  pattern CardanoEmulatorEraTx,
 )
import Ledger.Tx qualified as LedgerTx
import Core.Observers.Observer (stakeValidatorFromBytes)
import Plutus.Script.Utils.Address qualified as ScriptAddr
import Plutus.Script.Utils.Scripts (Language (PlutusV2), Versioned (..))
import Core.Api.AppContext (Env (..), defaultWalletResolver)
import Core.Api.State (ClientRegistrationStore, CompleteStore, PendingStore)
import Core.TxBuilder (buildTx)

-- | Create a Cooked environment for the Cavefish server using the provided
-- parameters.
mkCookedEnv ::
  TVar MockChainState ->
  PendingStore ->
  CompleteStore ->
  ClientRegistrationStore ->
  SecretKey ->
  PkeSecretKey ->
  Wallet ->
  NominalDiffTime ->
  Integer ->
  Env
mkCookedEnv mockState pendingStore completeStore clientRegStore spSk pkeSk spWallet ttl spFee = env
  where
    pkePk = toPublicKey pkeSk
    env =
      Env
        { spSk
        , pending = pendingStore
        , complete = completeStore
        , clientRegistration = clientRegStore
        , ttl
        , spWallet
        , resolveWallet = defaultWalletResolver
        , spFee
        , pkeSecret = pkeSk
        , pkePublic = pkePk
        , build = buildWithCooked mockState env
        , submit = submitWithCooked mockState env
        }

-- | Build a transaction using the Cooked mock chain.
buildWithCooked ::
  TVar MockChainState ->
  Env ->
  Intent ->
  ByteString ->
  IO BuildTxResult
buildWithCooked mockState env intent observerBytes = do
  st0 <- readTVarIO mockState
  let (result, st1) =
        runMockChainPure st0 $ do
          let stakeValidator = stakeValidatorFromBytes observerBytes
              cred =
                ScriptAddr.toCredential
                  (Versioned (getStakeValidator stakeValidator) PlutusV2)
          registerStakingCred cred 0 0
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
