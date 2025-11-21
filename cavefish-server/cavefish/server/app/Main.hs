-- | Cavefish server main module.
--  This module initializes and starts the Cavefish server, setting up the necessary
--  environment and configurations.
module Main where

import Control.Concurrent.STM (newTVarIO)
import Control.Monad.IO.Class (liftIO)
import Cooked (wallet)
import Core.Api.Config (
  Config (httpServer, wbps),
  HttpServer (HttpServer, port),
  Wbps (Wbps),
  loadConfig,
 )
import Core.CavefishLogEvent (CavefishLogEvent (LogSPConfigLoaded))
import Core.Logging (Verbosity (Verbose), traceWith, withTracer)
import Core.Pke (deriveSecretKey)
import Crypto.Error (CryptoFailable (CryptoFailed, CryptoPassed))
import Crypto.PubKey.Ed25519 qualified as Ed
import Data.ByteString qualified as BS
import Network.Wai.Handler.Warp qualified as Warp
import Paths_cavefish_server (getDataFileName)
import Sp.Emulator (initialMockState, mkCookedEnv)
import Sp.Server (mkApp)
import System.IO (hPutStrLn, stderr)
import WBPS.Core.FileScheme (mkFileSchemeFromRoot)

-- | The name of the configuration file
-- See relevant `data-file` entry in the .cabal file for more details.
configFileName :: FilePath
configFileName = "config/config.toml"

getConfigFileName :: FilePath -> IO FilePath
getConfigFileName = getDataFileName

main :: IO ()
main = (withTracer $ Verbose "SP.Server") $ \tr -> do
  configFilePath <-
    liftIO $
      getConfigFileName configFileName >>= \configFile -> do
        liftIO $ hPutStrLn stderr ("Using config file: " <> configFile) >> pure configFile
  config <- loadConfig configFilePath
  traceWith tr $ LogSPConfigLoaded config
  mockState <- liftIO $ newTVarIO initialMockState
  pendingStore <- liftIO $ newTVarIO mempty
  completeStore <- liftIO $ newTVarIO mempty
  clientStore <- liftIO $ newTVarIO mempty
  let (Wbps path) = wbps config
  wbpsScheme <- liftIO $ mkFileSchemeFromRoot path
  let HttpServer {port} = httpServer config
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
          spSk
          pkeSk
          (wallet 1)
          wbpsScheme
          config

  liftIO $
    Warp.run
      port
      (mkApp env)
