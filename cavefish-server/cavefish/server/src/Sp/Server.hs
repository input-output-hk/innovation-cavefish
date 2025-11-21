{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Sp.Server (
  CavefishApi,
  mkApp,
) where

import Core.Api.AppContext (AppM, Env, runApp)
import Core.Api.Messages (
  Accounts,
  CommitReq,
  CommitResp,
  PendingResp,
  TransactionResp,
  clientsH,
  commitH,
  pendingH,
  transactionH,
 )
import Core.SP.AskSubmission qualified as AskSubmission
import Core.SP.DemonstrateCommitment qualified as DemonstrateCommitment
import Core.SP.Register qualified as Register
import Data.Text (Text)
import Network.Wai (Application)
import Network.Wai.Middleware.Cors (
  CorsResourcePolicy (corsMethods, corsRequestHeaders),
  cors,
  simpleCorsResourcePolicy,
 )
import Servant (
  Capture,
  Get,
  HasServer (ServerT),
  JSON,
  Post,
  Proxy (Proxy),
  ReqBody,
  hoistServer,
  serve,
 )
import Servant.API ((:<|>) ((:<|>)), (:>))
import Sp.Middleware (cavefishMiddleware)

type CavefishApi =
  "register" :> ReqBody '[JSON] Register.Inputs :> Post '[JSON] Register.Outputs
    :<|> "demonstrateCommitment"
      :> ReqBody '[JSON] DemonstrateCommitment.Inputs
      :> Post '[JSON] DemonstrateCommitment.Outputs
    :<|> "askSubmission" :> ReqBody '[JSON] AskSubmission.Inputs :> Post '[JSON] AskSubmission.Outputs
    :<|> "commit" :> ReqBody '[JSON] CommitReq :> Post '[JSON] CommitResp
    :<|> "clients" :> Get '[JSON] Accounts
    :<|> "pending" :> Get '[JSON] PendingResp
    :<|> "transaction" :> Capture "id" Text :> Get '[JSON] TransactionResp

cavefishApi :: Proxy CavefishApi
cavefishApi = Proxy

mkApp :: Env -> Application
mkApp env =
  let
    policy =
      simpleCorsResourcePolicy
        { corsRequestHeaders = ["Content-Type"]
        , corsMethods = ["GET", "POST", "OPTIONS"]
        }
   in
    cors (const $ Just policy) $
      cavefishMiddleware $
        serve cavefishApi $
          hoistServer cavefishApi (runApp env) server

server :: ServerT CavefishApi AppM
server =
  Register.handle
    :<|> DemonstrateCommitment.handle
    :<|> AskSubmission.handle
    :<|> commitH
    :<|> clientsH
    :<|> pendingH
    :<|> transactionH
