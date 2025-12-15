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
) where

import Cavefish
import Cavefish.Api.ServerConfiguration
import Cavefish.Services.TxBuilding (ServiceFee, TxBuilding (..))
import Cavefish.Services.WBPS (WBPS (..))
import Control.Monad.IO.Class (MonadIO (..))
import Cooked (InitialDistribution)
import Cooked.MockChain
import Data.ByteString.Lazy.Char8 qualified as BL8
import Intent.Example.DSL
import Intent.Example.TxBuilder (buildTx)
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
  InitialDistribution ->
  ServiceFee ->
  IntentDSL ->
  m TxUnsigned
buildWithCooked initialDistribution serviceProviderFee intentDSL = do
  case toCanonicalIntent intentDSL of
    Left err -> liftIO $ fail ("buildTx failed: " <> show err)
    Right canonicalIntent -> do
      let mockChainReturnUnsignedTx = runMockChainFrom initialDistribution (buildTx canonicalIntent serviceProviderFee)
      case mcrValue mockChainReturnUnsignedTx of
        Left err -> liftIO $ fail ("buildTx failed: " <> show err)
        Right result -> return result
