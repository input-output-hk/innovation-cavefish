{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Sp.Server where

import Data.Text (Text)
import Servant
    ( Proxy(..),
      hoistServer,
      serve,
      type (:<|>)(..),
      Capture,
      JSON,
      ReqBody,
      type (:>),
      Get,
      Post,
      HasServer(ServerT),
      Application )
import Core.Api.AppContext (AppM, Env (..), runApp)
import Core.Api.Messages

type CavefishApi =
  "prepare" :> ReqBody '[JSON] PrepareReq :> Post '[JSON] PrepareResp
    {- The expected flow we require (after `prepare`):
        Signer (LC)                     Service Provider (SP)
        ----------------------------------------------------------------
        WBPS Execution for m := tx||auxnt        Produce commitment comtx
                                      (comtx, TxAbs) - PrepareResp
                                      <-----------
        Produce blind sig. com. `R = g^r`
                                            R - CommitReq
                                      ----------->
                                                Produce challenge `c` and proof `π`
                                          (c, π) - CommitResp
                                      <-----------
                Check proof `π`
                                            s - FinaliseReq
                Produce `s = r + cx`  -----------> Produce signature `σ = (R, s)`

      The final shape will be something like:

      "prepare" :: PrepareReq -> (comtx, TxAbs)
      "commit" :: R -> (c, π)
      "finalise" :: s -> FinaliseResp
    -}
    :<|> "commit" :> ReqBody '[JSON] CommitReq :> Post '[JSON] CommitResp
    :<|> "finalise" :> ReqBody '[JSON] FinaliseReq :> Post '[JSON] FinaliseResp
    :<|> "register" :> ReqBody '[JSON] RegisterReq :> Post '[JSON] RegisterResp
    :<|> "clients" :> Get '[JSON] ClientsResp
    :<|> "pending" :> Get '[JSON] PendingResp
    :<|> "transaction" :> Capture "id" Text :> Get '[JSON] TransactionResp

cavefishApi :: Proxy CavefishApi
cavefishApi = Proxy

mkApp :: Env -> Application
mkApp env = serve cavefishApi (hoistServer cavefishApi (runApp env) server)

server :: ServerT CavefishApi AppM
server = prepareH :<|> commitH :<|> finaliseH :<|> registerH :<|> clientsH :<|> pendingH :<|> transactionH
