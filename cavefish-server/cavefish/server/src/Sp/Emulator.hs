-- | Module providing an implementation of the transaction building and submission
--    functions using the Cooked mock chain.
--
--    This module defines functions to create a Cooked environment, build transactions,
--    and submit them to the mock chain. It also includes utilities to manage the
--    mock chain state.
module Sp.Emulator (
  mkServerContext,
) where

import Cavefish (CavefishServices (CavefishServices, txBuildingService, wbpsService))
import Cavefish.Api.ServerConfiguration (
  ServerConfiguration (ServerConfiguration, httpServer, serviceProviderFee, transactionExpiry, wbps),
 )
import Cavefish.Services.TxBuilding (ServiceFee, TxBuilding (TxBuilding, build, fees, submit))
import Cavefish.Services.WBPS (
  WBPS (WBPS, demonstrate, loadAccount, loadAccounts, loadCommitmentDemonstrationEvents, loadSession, prove, register),
 )
import Control.Monad.IO.Class (MonadIO (liftIO))
import Cooked (
  InitialDistribution,
  mcrValue,
  runMockChainFromInitDist,
 )
import Data.ByteString.Lazy.Char8 qualified as BL8 (pack)
import Intent.Example.DSL (IntentDSL, toCanonicalIntent)
import Intent.Example.TxBuilder (buildTx)
import Servant (
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
import WBPS.Core.Session.Demonstration.Artefacts.Cardano.UnsignedTx (UnsignedTx)
import WBPS.Core.Session.Demonstration.Demonstrate qualified as Demonstration
import WBPS.Core.Session.FetchSession qualified as SessionFetch
import WBPS.Core.Session.Proving.Prove qualified as Proving
import WBPS.Core.Setup.Circuit.FileScheme (FileScheme)
import WBPS.WBPS (runWBPS)

mkServerContext ::
  InitialDistribution ->
  FileScheme ->
  ServerConfiguration ->
  CavefishServices
mkServerContext
  initialDistribution
  wbpsScheme
  ServerConfiguration {..} =
    CavefishServices
      { txBuildingService =
          TxBuilding
            { fees = serviceProviderFee
            , build = buildWithCooked initialDistribution serviceProviderFee
            , submit = \_ -> pure ()
            }
      , wbpsService =
          WBPS
            { register = \userWalletPublicKey ->
                liftIO (runWBPS wbpsScheme (Registration.register userWalletPublicKey))
                  >>= \case
                    Left [AccountAlreadyRegistered _] -> throwError err422 {errBody = BL8.pack "Account Already Registered"}
                    Left e -> throwError err500 {errBody = BL8.pack ("Unexpected event" ++ show e)}
                    Right x -> pure x
            , demonstrate = \userWalletPublicKey tx ->
                liftIO (runWBPS wbpsScheme (Demonstration.demonstrate userWalletPublicKey tx))
                  >>= \case
                    (Left e) -> throwError err500 {errBody = BL8.pack ("Unexpected event" ++ show e)}
                    (Right x) -> pure x
            , prove = \userWalletPublicKey commitmentId bigR ->
                liftIO (runWBPS wbpsScheme (Proving.prove userWalletPublicKey commitmentId bigR))
                  >>= \case
                    Left [SessionNotFound _ _] -> throwError err404 {errBody = BL8.pack "Session Not Found"}
                    Left e -> throwError err500 {errBody = BL8.pack ("Unexpected event" ++ show e)}
                    Right x -> pure x
            , loadAccount = \userWalletPublicKey ->
                liftIO (runWBPS wbpsScheme (Registration.loadAccount userWalletPublicKey))
                  >>= \case
                    (Left e) -> throwError err500 {errBody = BL8.pack ("Unexpected event" ++ show e)}
                    Right x -> pure x
            , loadAccounts =
                liftIO (runWBPS wbpsScheme Registration.loadAccounts)
                  >>= \case
                    (Left e) -> throwError err500 {errBody = BL8.pack ("Unexpected event" ++ show e)}
                    Right x -> pure x
            , loadSession = \userWalletPublicKey commitmentId ->
                liftIO (runWBPS wbpsScheme (SessionFetch.loadExistingSession userWalletPublicKey commitmentId))
                  >>= \case
                    Left [SessionNotFound _ _] -> throwError err404 {errBody = BL8.pack "Session Not Found"}
                    Left e -> throwError err500 {errBody = BL8.pack ("Unexpected event" ++ show e)}
                    Right x -> pure x
            , loadCommitmentDemonstrationEvents = \userWalletPublicKey commitmentId ->
                liftIO (runWBPS wbpsScheme (SessionFetch.loadExistingCommitmentDemonstrationEvents userWalletPublicKey commitmentId))
                  >>= \case
                    Left [SessionNotFound _ _] -> throwError err404 {errBody = BL8.pack "Session Not Found"}
                    Left e -> throwError err500 {errBody = BL8.pack ("Unexpected event" ++ show e)}
                    Right x -> pure x
            }
      }

-- | Build a transaction using the Cooked mock chain.
buildWithCooked ::
  MonadIO m =>
  InitialDistribution ->
  ServiceFee ->
  IntentDSL ->
  m UnsignedTx
buildWithCooked initialDistribution serviceProviderFee intentDSL = do
  case toCanonicalIntent intentDSL of
    Left err -> liftIO $ fail ("buildTx failed: " <> show err)
    Right canonicalIntent -> do
      let mockChainReturnUnsignedTx = runMockChainFromInitDist initialDistribution (buildTx canonicalIntent serviceProviderFee)
      case mcrValue mockChainReturnUnsignedTx of
        Left err -> liftIO $ fail ("buildTx failed: " <> show err)
        Right result -> return result
