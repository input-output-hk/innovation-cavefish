module Main (main) where

import ClientBackend.Server (Env (..), mkApp)
import Network.Wai.Handler.Warp qualified as Warp

main :: IO ()
main = do
  putStrLn ("Starting Cavefish client backend on port " <> show 8081)
  let env = Env
  Warp.run 8081 (mkApp env)
