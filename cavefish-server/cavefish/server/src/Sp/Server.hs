{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Sp.Server (
  Cavefish,
  Register,
  mkServer,
) where

import Core.Api.Messages (
  PendingResp,
  TransactionResp,
  pendingH,
  transactionH,
 )
import Core.Api.ServerContext (ServerContext, ServerM, runApp)
import Core.SP.AskCommitmentProof qualified as AskCommitmentProof
import Core.SP.AskSubmission qualified as AskSubmission
import Core.SP.DemonstrateCommitment qualified as DemonstrateCommitment
import Core.SP.FetchAccount qualified as FetchAccount
import Core.SP.FetchAccounts qualified as FetchAccounts
import Core.SP.Register qualified as Register
import Data.Text (Text)
import Network.Wai (Application, Middleware)
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

type Cavefish =
  Register
    :<|> DemonstrateCommitment
    :<|> AskCommitmentProof
    :<|> "askSubmission" :> ReqBody '[JSON] AskSubmission.Inputs :> Post '[JSON] AskSubmission.Outputs
    :<|> FetchAccount
    :<|> FetchAccounts
    :<|> "pending" :> Get '[JSON] PendingResp
    :<|> "transaction" :> Capture "id" Text :> Get '[JSON] TransactionResp

type Register = "register" :> ReqBody '[JSON] Register.Inputs :> Post '[JSON] Register.Outputs

type DemonstrateCommitment =
  "demonstrateCommitment"
    :> ReqBody '[JSON] DemonstrateCommitment.Inputs
    :> Post '[JSON] DemonstrateCommitment.Outputs

type AskCommitmentProof =
  "askCommitmentProof"
    :> ReqBody '[JSON] AskCommitmentProof.Inputs
    :> Post '[JSON] AskCommitmentProof.Outputs

type FetchAccount =
  "fetchAccount" :> ReqBody '[JSON] FetchAccount.Inputs :> Post '[JSON] FetchAccount.Outputs

type FetchAccounts = "fetchAccounts" :> Get '[JSON] FetchAccounts.Outputs

cavefishApi :: Proxy Cavefish
cavefishApi = Proxy

mkServer :: Middleware -> ServerContext -> Application
mkServer cavefishMiddleware env =
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

server :: ServerT Cavefish ServerM
server =
  Register.handle
    :<|> DemonstrateCommitment.handle
    :<|> AskCommitmentProof.handle
    :<|> AskSubmission.handle
    :<|> FetchAccount.handle
    :<|> FetchAccounts.handle
    :<|> pendingH
    :<|> transactionH
