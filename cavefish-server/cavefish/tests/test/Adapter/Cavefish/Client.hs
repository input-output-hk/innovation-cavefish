module Adapter.Cavefish.Client (
  getServiceProviderAPI,
  mkApplication,
  ServiceProviderAPI (..),
) where

import Control.Concurrent.STM (newTVarIO)
import Cooked (InitialDistribution)
import Core.Api.ServerConfiguration (ServerConfiguration)
import Core.SP.AskCommitmentProof qualified as AskCommitmentProof
import Core.SP.DemonstrateCommitment qualified as DemonstrateCommitment
import Core.SP.FetchAccount qualified as FetchAccount
import Core.SP.FetchAccounts qualified as FetchAccounts
import Core.SP.Register qualified as Register
import Data.Default (def)
import Data.Map.Strict qualified as Map
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant (Application, Proxy (Proxy), type (:<|>) ((:<|>)))
import Servant.Client (BaseUrl (BaseUrl))
import Servant.Client qualified as SC
import Sp.Emulator (mkServerContext)
import Sp.Middleware (errStatusTraceMiddleware)
import Sp.Server (Cavefish, mkServer)
import Test.Hspec (expectationFailure)
import WBPS.Core.FileScheme (FileScheme)

getServiceProviderAPI :: Int -> IO ServiceProviderAPI
getServiceProviderAPI port = do
  manager <- newManager defaultManagerSettings
  let baseUrl = BaseUrl SC.Http "127.0.0.1" port ""
      ( register
          :<|> demonstrateCommitment
          :<|> askCommitmentProof
          :<|> _
          :<|> fetchAccount
          :<|> fetchAccounts
          :<|> _
          :<|> _
        ) = SC.client (Proxy @Cavefish)
  return
    ServiceProviderAPI
      { register = runClientOrFail (SC.mkClientEnv manager baseUrl) . register
      , demonstrateCommitment = runClientOrFail (SC.mkClientEnv manager baseUrl) . demonstrateCommitment
      , askCommitmentProof = runClientOrFail (SC.mkClientEnv manager baseUrl) . askCommitmentProof
      , fetchAccount = runClientOrFail (SC.mkClientEnv manager baseUrl) . fetchAccount
      , fetchAccounts = runClientOrFail (SC.mkClientEnv manager baseUrl) fetchAccounts
      }

data ServiceProviderAPI
  = ServiceProviderAPI
  { register :: Register.Inputs -> IO Register.Outputs
  , demonstrateCommitment :: DemonstrateCommitment.Inputs -> IO DemonstrateCommitment.Outputs
  , askCommitmentProof :: AskCommitmentProof.Inputs -> IO AskCommitmentProof.Outputs
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
  InitialDistribution ->
  Application
mkApplication wbpsScheme initialDistribution =
  let serverConfigurationByDefault :: ServerConfiguration = def
   in mkServer errStatusTraceMiddleware $
        mkServerContext
          initialDistribution
          wbpsScheme
          serverConfigurationByDefault
