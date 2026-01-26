-- | Module providing an implementation of the transaction building and submission
--    functions using the Cooked mock chain.
--
--    This module defines functions to create a Cooked environment, build transactions,
--    and submit them to the mock chain. It also includes utilities to manage the
--    mock chain state.
module Sp.Emulator (
  mkServerContext,
) where

import Cardano.Api qualified as Api
import Cardano.Node.Emulator.Internal.Node qualified as Emulator
import Cavefish (CavefishServices (CavefishServices, txBuildingService, wbpsService))
import Cavefish.Api.ServerConfiguration (
  ServerConfiguration (ServerConfiguration, httpServer, serviceProviderFee, transactionExpiry, wbps),
 )
import Cavefish.Services.TxBuilding (
  ServiceFee,
  TxBuilding (TxBuilding, build, fees, submit, txStatus),
  TxStatus (TxStatusSubmitted, TxStatusUnknown),
 )
import Cavefish.Services.WBPS (
  WBPS (WBPS, demonstrate, loadAllRegistered, loadCommitmentDemonstrationEvents, loadRegisteredMaybe, loadSession, prove, register, submit),
 )
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar)
import Control.Monad (forM_)
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.Except qualified as Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State.Strict (gets, modify', runStateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (runWriterT)
import Cooked.InitialDistribution (InitialDistribution)
import Cooked.MockChain.BlockChain (MockChainError (MCEUnsupportedFeature, MCEValidationError))
import Cooked.MockChain.Direct (
  MockChainConf (MockChainConf, mccFunOnResult, mccInitialDistribution, mccInitialState),
  MockChainT (unMockChain),
  runMockChainFromConf,
 )
import Cooked.MockChain.MockChainState (MockChainState (mcstLedgerState, mcstOutputs, mcstParams), addOutput, removeOutput)
import Cooked.Skeleton.Output (
  Payable (InlineDatum, Value, VisibleHashedDatum),
  PayableKind (IsDatum),
  TxSkelOut,
  receives,
  (<&&>),
 )
import Data.ByteString.Lazy.Char8 qualified as BL8 (pack)
import Data.Default (Default (def))
import Data.Functor ((<&>))
import Data.Map.Strict qualified as Map
import Intent.Example.DSL (IntentDSL, toCanonicalIntent)
import Intent.Example.TxBuilder (buildTx)
import Ledger.Address qualified as LedgerAddress
import Ledger.Index qualified as LedgerIndex
import Ledger.Tx qualified as LedgerTx
import Ledger.Tx.CardanoAPI qualified as LedgerApi
import PlutusLedgerApi.V1 qualified as PlutusV1
import PlutusLedgerApi.V3.Tx qualified as PlutusTx (TxOutRef (TxOutRef))
import Servant (
  ServerError,
  err404,
  err422,
  err500,
  errBody,
  throwError,
 )
import WBPS.Core.Failure (
  WBPSFailure (AccountAlreadyRegistered, SessionNotFound),
 )
import WBPS.Core.Registration.FetchAccounts qualified as Registration
import WBPS.Core.Registration.Register qualified as Registration
import WBPS.Core.Session.FetchSession qualified as SessionFetch
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Cardano.UnsignedTx (UnsignedTx)
import WBPS.Core.Session.Steps.Demonstration.Demonstrate qualified as Demonstration
import WBPS.Core.Session.Steps.Demonstration.Persistence.Events qualified as Demonstrated
import WBPS.Core.Session.Steps.Proving.Persistence.Events qualified as Proved
import WBPS.Core.Session.Steps.Proving.Prove qualified as Proving
import WBPS.Core.Session.Steps.Submitting.Artefacts.SubmittedTx (SubmitTx)
import WBPS.Core.Session.Steps.Submitting.Submit qualified as Submitting
import WBPS.Core.Setup.Circuit.FileScheme (FileScheme)

mkServerContext ::
  InitialDistribution ->
  FileScheme ->
  ServerConfiguration ->
  IO CavefishServices
mkServerContext
  initialDistribution
  wbpsScheme
  ServerConfiguration {..} = do
    mockChainStore <- newMockChainStore initialDistribution
    pure $
      CavefishServices
        { txBuildingService =
            txBuildingServiceInstance mockChainStore serviceProviderFee
        , wbpsService = wbpsServiceInstance wbpsScheme
        }

txBuildingServiceInstance :: MockChainStore -> ServiceFee -> TxBuilding
txBuildingServiceInstance mockChainStore serviceProviderFee =
  TxBuilding
    { fees = serviceProviderFee
    , build = buildWithCooked mockChainStore serviceProviderFee
    , submit = submitTx
    , txStatus = getTxStatus
    }
  where
    submitTx :: forall m. (MonadIO m, MonadError ServerError m) => Api.Tx Api.ConwayEra -> m ()
    submitTx tx =
      liftIO (runMockChainWithState mockChainStore (submitWithCooked tx))
        >>= \case
          Left err -> throwError err500 {errBody = BL8.pack ("MockChain submit failed: " <> show err)}
          Right () -> pure ()
    getTxStatus :: forall m. MonadIO m => Api.TxId -> m TxStatus
    getTxStatus txId = liftIO $ txStatusFromStore mockChainStore txId

wbpsServiceInstance :: FileScheme -> WBPS
wbpsServiceInstance wbpsScheme =
  WBPS
    { register = runWBPSWith wbpsScheme . Registration.register
    , demonstrate = \registrationId tx ->
        runWBPSWith wbpsScheme (Demonstration.demonstrate registrationId tx)
          <&> mapDemonstrateOutput
    , prove = \sessionId bigR ->
        runWBPSWith wbpsScheme (Proving.prove sessionId bigR)
          <&> mapProveOutput
    , submit = \sessionId submitTx' signature ->
        runWBPSWith wbpsScheme (Submitting.submit (liftSubmitTx submitTx') sessionId signature)
    , loadRegisteredMaybe = runWBPSWith wbpsScheme . Registration.loadRegisteredMaybe
    , loadAllRegistered = runWBPSWith wbpsScheme Registration.loadAllRegistered
    , loadSession = runWBPSWith wbpsScheme . SessionFetch.loadExistingSession
    , loadCommitmentDemonstrationEvents = \sessionId ->
        runWBPSWith wbpsScheme (Demonstrated.loadHistory sessionId)
          <&> mapCommitmentDemonstrationEvents
    }
  where
    mapDemonstrateOutput (sessionId, Demonstrated.EventHistory {demonstrated}) =
      (sessionId, demonstrated)
    mapProveOutput Proved.EventHistory {proved} = proved
    mapCommitmentDemonstrationEvents Demonstrated.EventHistory {registered, demonstrated} =
      (registered, demonstrated)

convertToServerError :: [WBPSFailure] -> ServerError
convertToServerError [SessionNotFound _] = err404 {errBody = BL8.pack "Session Not Found"}
convertToServerError [AccountAlreadyRegistered _] = err422 {errBody = BL8.pack "Account Already Registered"}
convertToServerError e = err500 {errBody = BL8.pack ("Unexpected event" ++ show e)}

handleWBPSResult :: MonadError ServerError m => Either [WBPSFailure] a -> m a
handleWBPSResult = either (throwError . convertToServerError) pure

runWBPSWith ::
  MonadError ServerError m =>
  FileScheme ->
  ReaderT FileScheme (ExceptT [WBPSFailure] m) a ->
  m a
runWBPSWith scheme action =
  runWBPSIn scheme action >>= handleWBPSResult

runWBPSIn ::
  FileScheme ->
  ReaderT FileScheme (ExceptT [WBPSFailure] m) a ->
  m (Either [WBPSFailure] a)
runWBPSIn scheme action =
  runExceptT $ runReaderT action scheme

liftSubmitTx ::
  Monad m =>
  SubmitTx m ->
  SubmitTx (ReaderT FileScheme (ExceptT [WBPSFailure] m))
liftSubmitTx submitTx signedTx =
  lift $ lift (submitTx signedTx)

-- | Build a transaction using the Cooked mock chain.
buildWithCooked ::
  MonadIO m =>
  MockChainStore ->
  ServiceFee ->
  IntentDSL ->
  m UnsignedTx
buildWithCooked mockChainStore serviceProviderFee intentDSL = do
  case toCanonicalIntent intentDSL of
    Left err -> liftIO $ fail ("buildTx failed: " <> show err)
    Right canonicalIntent -> do
      liftIO (runMockChainWithState mockChainStore (buildTx canonicalIntent serviceProviderFee))
        >>= \case
          Left err -> liftIO $ fail ("buildTx failed: " <> show err)
          Right result -> pure result

type MockChainStore = MVar MockChainState

newMockChainStore :: InitialDistribution -> IO MockChainStore
newMockChainStore initialDistribution =
  newMVar (initMockChainState initialDistribution)

initMockChainState :: InitialDistribution -> MockChainState
initMockChainState initialDistribution =
  runMockChainFromConf
    MockChainConf
      { mccInitialState = def
      , mccInitialDistribution = initialDistribution
      , mccFunOnResult = snd . fst
      }
    (pure ())

runMockChainWithState ::
  MockChainStore ->
  MockChainT IO a ->
  IO (Either MockChainError a)
runMockChainWithState store action =
  modifyMVar store $ \st -> do
    ((result, st'), _) <- runWriterT $ runStateT (runExceptT (unMockChain action)) st
    pure (st', result)

txStatusFromStore :: MockChainStore -> Api.TxId -> IO TxStatus
txStatusFromStore store txId = do
  st <- readMVar store
  pure (txStatusFromState st txId)

txStatusFromState :: MockChainState -> Api.TxId -> TxStatus
txStatusFromState st txId =
  let plutusTxId = LedgerApi.fromCardanoTxId txId
      hasTx =
        any
          (\(PlutusTx.TxOutRef outTxId _) -> outTxId == plutusTxId)
          (Map.keys (mcstOutputs st))
   in if hasTx then TxStatusSubmitted else TxStatusUnknown

submitWithCooked :: Api.Tx Api.ConwayEra -> MockChainT IO ()
submitWithCooked tx = do
  params <- gets mcstParams
  eLedgerState <- gets mcstLedgerState
  let cardanoTx = LedgerTx.CardanoEmulatorEraTx tx
      datumMap = LedgerTx.getCardanoTxData cardanoTx
  case Emulator.validateCardanoTx params eLedgerState cardanoTx of
    (_, LedgerIndex.FailPhase1 _ err) ->
      Except.throwError $ MCEValidationError LedgerIndex.Phase1 err
    (newELedgerState, LedgerIndex.FailPhase2 _ err _) -> do
      modify' (\st -> st {mcstLedgerState = newELedgerState})
      forM_ (LedgerTx.getCardanoTxCollateralInputs cardanoTx) $ \txIn ->
        modify' (removeOutput (LedgerApi.fromCardanoTxIn txIn))
      forM_ (Map.toList $ LedgerTx.getCardanoTxProducedReturnCollateral cardanoTx) $ \(txIn, txOut) -> do
        txSkelOut <- txSkelOutFromLedgerTxOut datumMap txOut
        modify' (addOutput (LedgerApi.fromCardanoTxIn txIn) txSkelOut)
      Except.throwError $ MCEValidationError LedgerIndex.Phase2 err
    (newELedgerState, LedgerIndex.Success {}) -> do
      modify' (\st -> st {mcstLedgerState = newELedgerState})
      forM_ (LedgerTx.getCardanoTxOutRefs cardanoTx) $ \(txOut, txIn) -> do
        txSkelOut <- txSkelOutFromLedgerTxOut datumMap txOut
        modify' (addOutput (LedgerApi.fromCardanoTxIn txIn) txSkelOut)
      forM_ (LedgerTx.getCardanoTxInputs cardanoTx) $ \txIn ->
        modify' (removeOutput (LedgerApi.fromCardanoTxIn txIn))

txSkelOutFromLedgerTxOut ::
  Map.Map PlutusV1.DatumHash PlutusV1.Datum ->
  LedgerTx.TxOut ->
  MockChainT IO TxSkelOut
txSkelOutFromLedgerTxOut datumMap (LedgerTx.TxOut (Api.TxOut addr value datum refScript)) = do
  pkh <-
    maybe
      (Except.throwError $ MCEUnsupportedFeature "script outputs are not supported")
      pure
      (LedgerAddress.cardanoPubKeyHash addr)
  datumPayable <- resolveDatum datumMap datum
  case refScript of
    Api.ReferenceScriptNone -> pure ()
    _ -> Except.throwError $ MCEUnsupportedFeature "reference scripts are not supported"
  let txValue = LedgerApi.fromCardanoValue (Api.txOutValueToValue value)
  pure $ case datumPayable of
    Nothing -> receives pkh (Value txValue)
    Just datumPayable' -> receives pkh (Value txValue <&&> datumPayable')

resolveDatum ::
  Map.Map PlutusV1.DatumHash PlutusV1.Datum ->
  Api.TxOutDatum Api.CtxTx Api.ConwayEra ->
  MockChainT IO (Maybe (Payable '[IsDatum]))
resolveDatum _ Api.TxOutDatumNone = pure Nothing
resolveDatum _ (Api.TxOutDatumInline _ scriptData) =
  pure . Just . InlineDatum $ LedgerApi.fromCardanoScriptData scriptData
resolveDatum datumMap datum@(Api.TxOutDatumHash _ _) =
  case LedgerApi.fromCardanoTxOutDatumHash datum >>= (`Map.lookup` datumMap) of
    Just (PlutusV1.Datum datumContent) ->
      pure . Just . VisibleHashedDatum $ datumContent
    Nothing ->
      Except.throwError $
        MCEUnsupportedFeature "datum hash without body is not supported"
resolveDatum _ (Api.TxOutSupplementalDatum _ scriptData) =
  pure . Just . VisibleHashedDatum $ LedgerApi.fromCardanoScriptData scriptData
