{-# LANGUAGE PatternSynonyms #-}

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
import Cooked.Skeleton (TxSkelOut, txSkelOutValue)
import Core.Intent (
  BuildTxResult (..),
  ChangeDelta,
  Intent,
 )
import Core.TxAbs (TxAbs (..), cardanoTxToTxAbs)
import Crypto.PubKey.Ed25519 (SecretKey)
import Data.ByteString (ByteString)
import Data.Default (def)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import Ledger.Scripts (StakeValidator (getStakeValidator))
import Ledger.Tx (
  pattern CardanoEmulatorEraTx,
 )
import Ledger.Tx.CardanoAPI (toCardanoValue)
import Observers.Observer (stakeValidatorFromBytes)
import Plutus.Script.Utils.Address qualified as ScriptAddr
import Plutus.Script.Utils.Scripts (Language (PlutusV2), Versioned (..))
import Sp.App (Env (..), defaultWalletResolver)
import Sp.State (ClientRegistrationStore, CompleteStore, PendingStore)
import Sp.TxBuilder (buildTx)

mkCookedEnv ::
  TVar MockChainState ->
  PendingStore ->
  CompleteStore ->
  ClientRegistrationStore ->
  SecretKey ->
  Wallet ->
  NominalDiffTime ->
  Integer ->
  Env
mkCookedEnv mockState pendingStore completeStore clientRegStore spSk spWallet ttl spFee = env
 where
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
      , build = buildWithCooked mockState env
      , submit = submitWithCooked mockState env
      }

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
      let txAbs = cardanoTxToTxAbs cardanoTx
          consumed = consumedTotal st0 st1
          produced = producedTotal (outputs txAbs)
          changeDelta = consumed <> Api.negateValue produced
      pure
        BuildTxResult
          { tx
          , changeDelta
          , txAbs
          , mockState = st1
          }

submitWithCooked ::
  TVar MockChainState ->
  Env ->
  Api.Tx Api.ConwayEra ->
  MockChainState ->
  IO (Either Text ())
submitWithCooked mockState _env _tx newState = do
  atomically $ writeTVar mockState newState
  pure (Right ())

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

consumedTotal ::
  MockChainState ->
  MockChainState ->
  ChangeDelta
consumedTotal stateBefore stateAfter =
  mconcat
    [ toDelta out
    | (oref, (out, True)) <- Map.toList (mcstOutputs stateBefore)
    , let mAfter = Map.lookup oref (mcstOutputs stateAfter)
    , consumedInAfter mAfter
    ]
 where
  toDelta :: TxSkelOut -> ChangeDelta
  toDelta txOut =
    case toCardanoValue (txSkelOutValue txOut) of
      Left err -> error ("failed to convert consumed output value: " <> show err)
      Right valueInEra -> valueInEra

consumedInAfter :: Maybe (TxSkelOut, Bool) -> Bool
consumedInAfter Nothing = True
consumedInAfter (Just (_, present)) = not present

producedTotal ::
  [Api.TxOut Api.CtxTx Api.ConwayEra] ->
  ChangeDelta
producedTotal outs =
  mconcat
    [ Api.txOutValueToValue val
    | Api.TxOut _ val _ _ <- outs
    ]

initialMockState :: MockChainState
initialMockState =
  case runMockChainPure def mockChainState0 of
    (Left err, _) -> error ("failed to initialise mock chain state: " <> show err)
    (Right st, _) -> st
