{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module ClientBackend.Api (
  Env (..),
  ClientApi,
  clientApi,
  mkApp,
  server,
) where

import ClientBackend.App (AppM, Env (Env), runApp)
import ClientBackend.Handlers (
  commitHelperH,
  demoAddressesH,
  finaliseHelperH,
  payToIntentH,
  prepareHelperH,
  registerHelperH,
  satisfiesH,
  verifyH,
 )
import ClientBackend.Types (
  CommitHelperReq,
  CommitHelperResp,
  DemoAddressesResp,
  FinaliseHelperReq,
  FinaliseHelperResp,
  IntentHelperResp,
  PayToIntentReq,
  PrepareHelperReq,
  PrepareHelperResp,
  RegisterHelperReq,
  RegisterHelperResp,
  SatisfiesReq,
  SatisfiesResp,
  VerifyReq,
  VerifyResp,
 )
import Network.Wai (Application)
import Network.Wai.Middleware.Cors (
  CorsResourcePolicy (corsMethods, corsRequestHeaders),
  cors,
  simpleCorsResourcePolicy,
 )
import Servant (
  Get,
  HasServer (ServerT),
  JSON,
  Post,
  Proxy (Proxy),
  ReqBody,
  hoistServer,
  serve,
  type (:<|>) ((:<|>)),
  type (:>),
 )

type ClientApi =
  "verify" :> ReqBody '[JSON] VerifyReq :> Post '[JSON] VerifyResp
    :<|> "satisfies" :> ReqBody '[JSON] SatisfiesReq :> Post '[JSON] SatisfiesResp
    :<|> "helpers" :> "register" :> ReqBody '[JSON] RegisterHelperReq :> Post '[JSON] RegisterHelperResp
    :<|> "helpers" :> "prepare" :> ReqBody '[JSON] PrepareHelperReq :> Post '[JSON] PrepareHelperResp
    :<|> "helpers" :> "commit" :> ReqBody '[JSON] CommitHelperReq :> Post '[JSON] CommitHelperResp
    :<|> "helpers" :> "finalise" :> ReqBody '[JSON] FinaliseHelperReq :> Post '[JSON] FinaliseHelperResp
    :<|> "helpers" :> "intent" :> "payto" :> ReqBody '[JSON] PayToIntentReq :> Post '[JSON] IntentHelperResp
    :<|> "helpers" :> "addresses" :> Get '[JSON] DemoAddressesResp

clientApi :: Proxy ClientApi
clientApi = Proxy

mkApp :: Env -> Application
mkApp env =
  let policy =
        simpleCorsResourcePolicy
          { corsRequestHeaders = ["Content-Type"]
          , corsMethods = ["GET", "POST", "OPTIONS"]
          }
   in cors (const $ Just policy) $
        serve clientApi (hoistServer clientApi (runApp env) server)

server :: ServerT ClientApi AppM
server =
  verifyH
    :<|> satisfiesH
    :<|> registerHelperH
    :<|> prepareHelperH
    :<|> commitHelperH
    :<|> finaliseHelperH
    :<|> payToIntentH
    :<|> demoAddressesH
