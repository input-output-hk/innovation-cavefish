-- | Cavefish server main module.
--
--  This module initializes and starts the Cavefish server, setting up the necessary
--  environment and configurations.
module Main where

import Control.Concurrent.STM (newTVarIO)
import Cooked (wallet)
import Core.Pke (deriveSecretKey)
import Crypto.Error (CryptoFailable (..))
import Crypto.PubKey.Ed25519 qualified as Ed
import Data.ByteString qualified as BS
import Network.Wai.Handler.Warp qualified as Warp
import Sp.Emulator (initialMockState, mkCookedEnv)
import Sp.Server (mkApp)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  let port = 8080
      ttlSeconds = 3600 :: Integer
      spFee = 0

  hPutStrLn stderr ("Starting Cavefish server on port " <> show port)

  mockState <- newTVarIO initialMockState
  pendingStore <- newTVarIO mempty
  completeStore <- newTVarIO mempty
  clientStore <- newTVarIO mempty

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
          (fromInteger ttlSeconds)
          spFee

  Warp.run port (mkApp env)
