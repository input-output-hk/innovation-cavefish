{- | This module defines a typeclass 'MonadBlockchain' for interacting with the Cardano blockchain,
  along with an implementation 'MonadBlockchainCardanoNodeT' that uses a Cardano node for communication.
  It also defines error types for handling various validation errors that may occur during transaction submission.
  The module provides functions to send transactions, query UTxOs, protocol parameters, system start time, era history, and current slot information.
-}

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Core.Cardano.Class (

  -- * Monad blockchain
  MonadBlockchain (..),
  trySendTx,
  singleUTxO,

  -- * Implementation
  MonadBlockchainCardanoNodeT (..),
  runMonadBlockchainCardanoNodeT,

   -- * Other types
  ExUnitsError (..),
  AsExUnitsError (..),
  ValidationError (..),
  AsValidationError (..),
  BlockchainException (..),

) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Set (Set)
import Cardano.Slotting.EpochInfo.API (
  epochInfoSlotToUTCTime,
  hoistEpochInfo,
 )
import Cardano.Api qualified as C
import Control.Exception (Exception, throwIO)
import Control.Monad.Except (
  MonadError,
  runExceptT,
 )
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (
  MonadTrans,
  ReaderT(runReaderT),
  ask,
  asks,
  lift,
 )
import Ouroboros.Network.Protocol.LocalStateQuery.Type qualified as T
import Control.Monad.IO.Class (liftIO)
import Control.Lens (Prism')
import Cardano.Ledger.Alonzo.Plutus.Evaluate (CollectError)
import Control.Lens.TH (makeClassyPrisms)
import Cardano.Ledger.Shelley.API (ApplyTxError)
-- import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Monad.State qualified as LazyState
import Control.Monad.State.Strict qualified as StrictState
import Control.Monad.Trans.Except (ExceptT)
import Cardano.Slotting.Time (SlotLength, SystemStart)
import Data.Time.Clock (UTCTime)
import Ouroboros.Consensus.HardFork.History (
  interpretQuery,
  slotToSlotLength,
 )
import Data.Text qualified as Text
import Data.Bifunctor (Bifunctor (first))

-- Error types
data ExUnitsError era
  = Phase1Error (C.TransactionValidityError era)
  | Phase2Error C.ScriptExecutionError
  deriving stock (Show)
  deriving anyclass (Exception)

makeClassyPrisms ''ExUnitsError



{- | Validation errors that can occur when submitting a transaction to the Cardano blockchain.
-}
data ValidationError era
  = ValidationErrorInMode !C.TxValidationErrorInCardanoMode
  -- ^ Error during transaction submission in Cardano mode.
  | VExUnits !(ExUnitsError era)
  -- ^ Error related to execution units during transaction validation.
  | PredicateFailures ![CollectError (C.ShelleyLedgerEra era)]
  -- ^ Predicate failures during transaction validation.
  | ApplyTxFailure !(ApplyTxError (C.ShelleyLedgerEra era))
  -- ^ Failure when applying the transaction to the ledger state.
  deriving anyclass (Exception)

{- | Show instance for 'ValidationError' that provides a human-readable representation of the error.
-}
instance (C.IsAlonzoBasedEra era) => Show (ValidationError era) where
  show err =
    C.alonzoEraOnwardsConstraints @era C.alonzoBasedEra $
      "ValidationError: " <> case err of
        ValidationErrorInMode err' -> "ValidationErrorInMode: " <> show err'
        VExUnits err' -> "VExUnits: " <> show err'
        PredicateFailures errs' -> "PredicateFailures: " <> show errs'
        ApplyTxFailure err' -> "ApplyTxFailure: " <> show err'

-- | Generate prisms for ValidationError
makeClassyPrisms ''ValidationError

-- | Instance declaration for AsExUnitsError typeclass
instance AsExUnitsError (ValidationError era) era where
  _ExUnitsError :: Prism' (ValidationError era) (ExUnitsError era)
  _ExUnitsError = _VExUnits . _ExUnitsError


-- | Exception type for blockchain-related errors
newtype BlockchainException = BlockchainException String
  deriving stock (Show)
  deriving anyclass (Exception)

-- | Monad transformer for Cardano node interactions
newtype MonadBlockchainCardanoNodeT era m a = MonadBlockchainCardanoNodeT
  { unMonadBlockchainCardanoNodeT :: ReaderT C.LocalNodeConnectInfo m a
  }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadError e, MonadFail)

-- | Run the MonadBlockchainCardanoNodeT with the given local node connection info
runMonadBlockchainCardanoNodeT :: C.LocalNodeConnectInfo -> MonadBlockchainCardanoNodeT era m a -> m a
runMonadBlockchainCardanoNodeT info (MonadBlockchainCardanoNodeT action) = runReaderT action info


-- | Typeclass for monads that can interact with the Cardano blockchain
class (Monad m) => MonadBlockchain era m | m -> era where
  -- | Send a transaction to the network.
  sendTx :: C.Tx era -> m (Either (ValidationError era) C.TxId)

-- |  Resolve tx inputs
  utxoByTxIn :: Set C.TxIn -> m (C.UTxO era)

  -- | Query the protocol parameters from the node
  queryProtocolParameters :: m (C.LedgerProtocolParameters era)
  -- | Query the system start time from the node
  querySystemStart :: m SystemStart
  -- | Query the era history from the node
  queryEraHistory :: m C.EraHistory
  -- | Query the current slot number, slot length and begin utc time for slot.
  querySlotNo :: m (C.SlotNo, SlotLength, UTCTime)
    -- ^ returns the current slot number, slot length and begin utc time for slot.
    -- Slot 0 is returned when at genesis.

  queryNetworkId :: m C.NetworkId
    -- ^ Get the network id

  -- Default implementations for MonadTrans instances
  default sendTx :: (MonadTrans t, m ~ t n, MonadBlockchain era n) => C.Tx era -> m (Either (ValidationError era) C.TxId)
  sendTx = lift . sendTx

  default utxoByTxIn :: (MonadTrans t, m ~ t n, MonadBlockchain era n) => Set C.TxIn -> m (C.UTxO era)
  utxoByTxIn txins = lift (utxoByTxIn txins)

  default queryProtocolParameters :: (MonadTrans t, m ~ t n, MonadBlockchain era n) => m (C.LedgerProtocolParameters era)
  queryProtocolParameters = lift queryProtocolParameters

  default querySystemStart :: (MonadTrans t, m ~ t n, MonadBlockchain era n) => m SystemStart
  querySystemStart = lift querySystemStart

  default queryEraHistory :: (MonadTrans t, m ~ t n, MonadBlockchain era n) => m C.EraHistory
  queryEraHistory = lift queryEraHistory

  default querySlotNo :: (MonadTrans t, m ~ t n, MonadBlockchain era n) => m (C.SlotNo, SlotLength, UTCTime)
  querySlotNo = lift querySlotNo

  default queryNetworkId :: (MonadTrans t, m ~ t n, MonadBlockchain era n) => m C.NetworkId
  queryNetworkId = lift queryNetworkId

instance (MonadBlockchain era m) => MonadBlockchain era (ExceptT e m)
instance (MonadBlockchain era m) => MonadBlockchain era (ReaderT e m)
instance (MonadBlockchain era m) => MonadBlockchain era (StrictState.StateT e m)
instance (MonadBlockchain era m) => MonadBlockchain era (LazyState.StateT e m)

-- | MonadBlockchain instance for MonadBlockchainCardanoNodeT
instance (MonadIO m, C.IsShelleyBasedEra era) => MonadBlockchain era (MonadBlockchainCardanoNodeT era m) where
  sendTx tx = MonadBlockchainCardanoNodeT $ do
    let txId = C.getTxId (C.getTxBody tx)
    info <- ask
    result <- liftIO (C.submitTxToNodeLocal info (C.TxInMode C.shelleyBasedEra tx))
    pure $ case result of
      C.SubmitSuccess ->
        Right txId
      C.SubmitFail reason ->
        Left $ ValidationErrorInMode reason

  utxoByTxIn txIns =
       runQuery' (C.QueryInEra (C.QueryInShelleyBasedEra C.shelleyBasedEra (C.QueryUTxO (C.QueryUTxOByTxIn txIns))))

  queryProtocolParameters =
    C.LedgerProtocolParameters <$> runQuery' (C.QueryInEra (C.QueryInShelleyBasedEra C.shelleyBasedEra C.QueryProtocolParameters))

  querySystemStart = runQuery C.QuerySystemStart

  queryEraHistory = runQuery C.QueryEraHistory
  querySlotNo = do
    (eraHistory@(C.EraHistory interpreter), systemStart) <- (,) <$> queryEraHistory <*> querySystemStart
    slotNo <-
      runQuery C.QueryChainPoint >>= \case
        C.ChainPointAtGenesis -> pure $ fromIntegral (0 :: Integer)
        C.ChainPoint slot _hsh -> pure slot
    MonadBlockchainCardanoNodeT $ do
      let logErr err = do
            let msg = "querySlotNo: Failed with " <> err
            liftIO $ throwIO $ BlockchainException msg
      utctime <- either logErr pure (slotToUtcTime eraHistory systemStart slotNo)
      either (logErr . show) (\l -> pure (slotNo, l, utctime)) (interpretQuery interpreter $ slotToSlotLength slotNo)

  queryNetworkId = MonadBlockchainCardanoNodeT (asks C.localNodeNetworkId)

-- | Run a query against the Cardano node
runQuery :: (MonadIO m) => C.QueryInMode a -> MonadBlockchainCardanoNodeT era m a
runQuery qry = MonadBlockchainCardanoNodeT $ do
  info <- ask
  result <- liftIO (runExceptT $ C.queryNodeLocalState info T.VolatileTip qry)
  case result of
    Left err -> do
      let msg = "runQuery: Query failed: " <> show err
      liftIO $ throwIO $ BlockchainException msg
    Right result' -> do
      pure result'

-- | Run a query that may result in an era mismatch error
runQuery' :: (MonadIO m, Show e1) => C.QueryInMode (Either e1 a) -> MonadBlockchainCardanoNodeT era m a
runQuery' qry =
  runQuery qry >>= \case
    Left err -> MonadBlockchainCardanoNodeT $ do
      let msg = "runQuery': Era mismatch: " <> show err
      liftIO $ throwIO $ BlockchainException msg
    Right result' -> pure result'

-- | Convert a slot number to UTC time
slotToUtcTime :: C.EraHistory -> C.SystemStart -> C.SlotNo -> Either String UTCTime
slotToUtcTime (C.toLedgerEpochInfo -> C.LedgerEpochInfo info) systemStart slot =
  epochInfoSlotToUTCTime (hoistEpochInfo (first Text.unpack) info) systemStart slot

-- | Send a transaction and throw an error if it fails.
trySendTx :: (MonadBlockchain era m, C.IsAlonzoBasedEra era) => C.Tx era -> m C.TxId
trySendTx = fmap (either (error . show) id) . sendTx

-- | Look up  a single UTxO
singleUTxO :: (MonadBlockchain era m) => C.TxIn -> m (Maybe (C.TxOut C.CtxUTxO era))
singleUTxO txi =
  utxoByTxIn (Set.singleton txi) >>= \case
    C.UTxO (Map.toList -> [(_, o)]) -> pure (Just o)
    _ -> pure Nothing
