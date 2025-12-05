-- | Cavefish server main module.
--  This module initializes and starts the Cavefish server, setting up the necessary
--  environment and configurations.
module Main where

import Control.Concurrent.STM (newTVarIO)
import Control.Monad.IO.Class (liftIO)
import Core.Api.Config (
  Config (httpServer, wbps),
  HttpServer (HttpServer, port),
  Wbps (Wbps),
  loadConfig,
 )
import Core.CavefishLogEvent (CavefishLogEvent (LogSPConfigLoaded))
import Core.Logging (Verbosity (Verbose), traceWith, withTracer)
import Network.Wai.Handler.Warp qualified as Warp
import Paths_cavefish_server (getDataFileName)
import Sp.Emulator (initialMockState, mkCookedEnv)
import Sp.Middleware (cavefishMiddleware)
import Sp.Server (mkApp)
import System.IO (hPutStrLn, stderr)
import WBPS.Core.FileScheme (mkFileSchemeFromRoot)

-- | The name of the configuration file
-- See relevant `data-files` entry in the .cabal file for more details.
configFileName :: FilePath
configFileName = "config/config.toml"

getConfigFileName :: FilePath -> IO FilePath
getConfigFileName = getDataFileName

main :: IO ()
main = withTracer (Verbose "SP.Server") $ \tr -> do
  configFilePath <-
    liftIO $
      getConfigFileName configFileName >>= \configFile -> do
        liftIO $ hPutStrLn stderr ("Using config file: " <> configFile) >> pure configFile
  config <- loadConfig configFilePath
  traceWith tr $ LogSPConfigLoaded config
  mockState <- liftIO $ newTVarIO initialMockState
  pendingStore <- liftIO $ newTVarIO mempty
  completeStore <- liftIO $ newTVarIO mempty

  let (Wbps path) = wbps config
  wbpsScheme <- liftIO $ mkFileSchemeFromRoot path
  let HttpServer {port} = httpServer config
      env =
        mkCookedEnv
          mockState
          pendingStore
          completeStore
          wbpsScheme
          config

  liftIO $
    Warp.run
      port
      (mkApp cavefishMiddleware env)
