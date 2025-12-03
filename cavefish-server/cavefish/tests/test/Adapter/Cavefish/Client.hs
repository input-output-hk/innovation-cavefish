module Adapter.Cavefish.Client (
  getServiceProviderAPI,
  mkApplication,
  ServiceProviderAPI (..),
) where

import Control.Concurrent.STM (newTVarIO)
import Core.Api.Config (Config)
import Core.SP.FetchAccount qualified as FetchAccount
import Core.SP.FetchAccounts qualified as FetchAccounts
import Core.SP.Register qualified as Register
import Data.Default (def)
import Data.Map.Strict qualified as Map
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant (Application, Proxy (Proxy), type (:<|>) ((:<|>)))
import Servant.Client (BaseUrl (BaseUrl))
import Servant.Client qualified as SC
import Sp.Emulator (initialMockState, mkCookedEnv)
import Sp.Middleware (errStatusTraceMiddleware)
import Sp.Server (CavefishApi, mkApp)
import Test.Hspec (expectationFailure)
import WBPS.Core.FileScheme (FileScheme)

getServiceProviderAPI :: Int -> IO ServiceProviderAPI
getServiceProviderAPI port = do
  manager <- newManager defaultManagerSettings
  let baseUrl = BaseUrl SC.Http "127.0.0.1" port ""

      ( register
          :<|> _
          :<|> _
          :<|> _
          :<|> fetchAccount
          :<|> fetchAccounts
          :<|> _
          :<|> _
        ) = SC.client (Proxy @CavefishApi)
  return
    ServiceProviderAPI
      { register = runClientOrFail (SC.mkClientEnv manager baseUrl) . register
      , fetchAccount = runClientOrFail (SC.mkClientEnv manager baseUrl) . fetchAccount
      , fetchAccounts = runClientOrFail (SC.mkClientEnv manager baseUrl) fetchAccounts
      }

data ServiceProviderAPI
  = ServiceProviderAPI
  { register :: Register.Inputs -> IO Register.Outputs
  , fetchAccount :: FetchAccount.Inputs -> IO FetchAccount.Outputs
  , fetchAccounts :: IO FetchAccounts.Outputs
  }

runClientOrFail :: SC.ClientEnv -> SC.ClientM a -> IO a
runClientOrFail clientEnv action = do
  SC.runClientM action clientEnv
    >>= \case
      Left err -> expectationFailure ("HTTP client call failed: " <> show err) >> fail "http client failure"
      Right value -> pure value

mkApplication ::
  FileScheme ->
  IO Application
mkApplication wbpsScheme = do
  pendingStore <- newTVarIO Map.empty
  completeStore <- newTVarIO Map.empty
  mockStateVar <- newTVarIO initialMockState
  let config :: Config = def
  pure . mkApp errStatusTraceMiddleware $
    mkCookedEnv
      mockStateVar
      pendingStore
      completeStore
      wbpsScheme
      config
