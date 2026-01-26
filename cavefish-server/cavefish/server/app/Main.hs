{-# LANGUAGE QuasiQuotes #-}

-- | Cavefish server main module.
--  This module initializes and starts the Cavefish server, setting up the necessary
--  environment and configurations.
module Main where

import Adapter.Logging (Verbosity (Verbose), traceWith, withTracer)
import Cavefish.Api.ServerConfiguration (
  HttpServer (HttpServer, port),
  ServerConfiguration (httpServer),
  loadConfig,
 )
import Cavefish.Performance.LogEvent (CavefishLogEvent (LogSPConfigLoaded))
import Control.Monad.IO.Class (liftIO)
import Data.Default (Default (def))
import Network.Wai.Handler.Warp qualified as Warp
import Path (Abs, Dir, Path, parseAbsDir, parseRelDir, reldir, (</>))
import Path.IO (getCurrentDir)
import Paths_cavefish_server (getDataFileName)
import Sp.Emulator (mkServerContext)
import Sp.Middleware (cavefishMiddleware)
import Sp.Server (mkServer)
import System.FilePath (isAbsolute)
import System.IO (hPutStrLn, stderr)
import WBPS.Core.Setup.Circuit.FileScheme (mkFileSchemeFromRoot)

-- | The name of the configuration file
-- See relevant `data-files` entry in the .cabal file for more details.
configFileName :: FilePath
configFileName = "config/config.toml"

getConfigFileName :: FilePath -> IO FilePath
getConfigFileName = getDataFileName

resolveRootDir :: FilePath -> IO (Path Abs Dir)
resolveRootDir raw
  | isAbsolute raw = parseAbsDir raw
  | otherwise = do
      cwd <- getCurrentDir
      rel <- parseRelDir raw
      pure (cwd </> rel)

main :: IO ()
main = withTracer (Verbose "SP.Server") $ \tr -> do
  configFilePath <-
    liftIO $
      getConfigFileName configFileName >>= \configFile -> do
        liftIO $ hPutStrLn stderr ("Using config file: " <> configFile) >> pure configFile
  config <- loadConfig configFilePath
  traceWith tr $ LogSPConfigLoaded config

  wbpsScheme <- liftIO $ mkFileSchemeFromRoot [reldir|production|]
  let HttpServer {port} = httpServer config
  env <-
    mkServerContext
      def
      wbpsScheme
      config

  liftIO $
    Warp.run
      port
      (mkServer cavefishMiddleware env)
