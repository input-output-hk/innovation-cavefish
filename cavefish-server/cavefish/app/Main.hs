module Main where

import Control.Concurrent.STM (newTVarIO)
import Cooked (wallet)
import Crypto.Error (CryptoFailable (..))
import qualified Crypto.PubKey.Ed25519 as Ed
import qualified Data.ByteString as BS
import qualified Network.Wai.Handler.Warp as Warp
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
      env =
        mkCookedEnv
          mockState
          pendingStore
          completeStore
          clientStore
          spSk
          (wallet 1)
          (fromInteger ttlSeconds)
          spFee

  Warp.run port (mkApp env)
