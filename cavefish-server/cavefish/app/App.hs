import Api.Api
import Servant.Server (Application, serve)
import Data.Proxy (Proxy (Proxy))
import Network.Wai.Handler.Warp (run)
import GHC.Clock (getMonotonicTimeNSec)

main :: IO ()
main = do
  t0 <- getMonotonicTimeNSec
  let env = Env (toInteger t0)
  run 4000 (app env)

app :: Env -> Application
app e = serve (Proxy :: Proxy CavefishApi) (server e)
