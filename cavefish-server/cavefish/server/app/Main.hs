-- | Cavefish server main module.
-- {-# OPTIONS_GHC -Wno-unused-imports #-}
--  This module initializes and starts the Cavefish server, setting up the necessary
--  environment and configurations.
module Main where

import Blammo.Logging.Simple (
  Message ((:#)),
  defaultLogSettings,
  logInfo,
  newLogger,
  runSimpleLoggingT,
  (.=),
 )
import Control.Concurrent.STM (newTVarIO)
import Control.Monad.IO.Class (liftIO)
import Cooked (wallet)
import Core.Api.AppContext (HttpServerConfig (port), httpServerConfig, waiMiddleware)
import Core.Pke (deriveSecretKey)
import Crypto.Error (CryptoFailable (CryptoFailed, CryptoPassed))
import Crypto.PubKey.Ed25519 qualified as Ed
import Data.ByteString qualified as BS
import Network.Wai.Handler.Warp qualified as Warp
import Sp.Emulator (initialMockState, mkCookedEnv)
import Sp.Server (mkApp, withRequestLogging)
import System.IO (hPutStrLn, stderr)
import WBPS.Core.FileScheme (mkFileSchemeFromRoot)

main :: IO ()
main = runSimpleLoggingT $ do
  mockState <- liftIO $ newTVarIO initialMockState
  pendingStore <- liftIO $ newTVarIO mempty
  completeStore <- liftIO $ newTVarIO mempty
  clientStore <- liftIO $ newTVarIO mempty
  let wbpsRoot = "wbps" -- TODO WG: Probably get this from some sort of config
  wbpsScheme <- liftIO $ mkFileSchemeFromRoot wbpsRoot
  logger <- liftIO $ newLogger defaultLogSettings
  liftIO $ hPutStrLn stderr ("Using WBPS root: " <> wbpsRoot)
  logInfo "Starting Cavefish Server"

  let spSk =
        case Ed.secretKey (BS.pack [1 .. 32]) of
          CryptoPassed sk -> sk
          CryptoFailed err -> error ("failed to derive service key: " <> show err)
      pkeSk =
        case deriveSecretKey (BS.pack [33 .. 64]) of
          Left err -> error ("failed to derive PKE secret key: " <> show err)
          Right sk -> sk

      env =
        mkCookedEnv
          mockState
          pendingStore
          completeStore
          clientStore
          spSk
          pkeSk
          (wallet 1)
          wbpsScheme
          logger
      app = mkApp env

  logInfo $ "Cavefish HTTP Server" :# ["configuration" .= (httpServerConfig env)]

  liftIO
    $ Warp.run
      (port . httpServerConfig $ env)
    $ withRequestLogging env (waiMiddleware env app)
