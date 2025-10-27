-- | Client implementation using a mock client to interact with the server.
--
--  This module defines the `ClientM` monad and associated functions to perform
--  client operations such as preparing intents, validating proofs, finalizing
--  transactions, and listing pending requests and registered clients.
module Client.Impl (
  ClientEnv (..),
  ClientM,
  runClient,
  withSession,
  ClientSession (..),
  startSession,
  throw422,
  eitherAs422,
  prepare,
  prepareAndValidate,
  finalise,
  runIntent,
  listPending,
  listClients,
) where

import Client.Mock (
  MockClient,
  RunServer,
  as422,
  finaliseWithClient,
  getClients,
  getPending,
  initMockClient,
  prepareWithClient,
  register,
  verifyPrepareProofWithClient,
  verifySatisfies,
 )
import Control.Monad.Except (MonadError, liftEither, throwError)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Trans.Class (lift)
import Core.Intent (IntentW)
import Crypto.PubKey.Ed25519 (PublicKey, SecretKey)
import Data.Bifunctor (first)
import Data.Text (Text)
import Servant (Handler, ServerError)
import Sp.Server (ClientsResp, FinaliseResp, PendingResp, PrepareResp)

-- | Monad for client operations against the server.
newtype ClientM a = ClientM (ReaderT ClientEnv Handler a)
  deriving newtype (Functor, Applicative, Monad, MonadReader ClientEnv, MonadError ServerError)

-- | Environment required to run client operations against the server.
data ClientEnv = ClientEnv
  { run :: RunServer
  , lcSk :: SecretKey
  , spPk :: PublicKey
  }

-- | Represents a client session with the server.
newtype ClientSession = ClientSession
  { client :: MockClient
  }

-- | Run a ClientM action with the given ClientEnv.
runClient :: ClientEnv -> ClientM a -> Handler a
runClient env (ClientM m) = runReaderT m env

-- | Create a client session and run the given action within that session.
withSession ::
  -- | Environment for the client
  ClientEnv ->
  -- | Action to run with the session
  (ClientSession -> ClientM a) ->
  Handler a
withSession env action = runClient env (startSession >>= action)

startSession :: ClientM ClientSession
startSession = do
  ClientEnv {run, lcSk, spPk} <- ask
  let unregistered = initMockClient run lcSk spPk
  client <- liftHandler (register unregistered)
  pure (ClientSession client)

throw422 :: Text -> ClientM a
throw422 = throwError . as422

eitherAs422 :: Either Text a -> ClientM a
eitherAs422 = liftEither . first as422

-- | Prepare an intent with the server.
prepare :: ClientSession -> IntentW -> ClientM PrepareResp
prepare ClientSession {client} intent = liftHandler (prepareWithClient client intent)

prepareAndValidate :: ClientSession -> IntentW -> ClientM PrepareResp
prepareAndValidate session intent = do
  resp <- prepare session intent
  eitherAs422 $
    (verifySatisfies intent resp >>= ensure "Satisfies failed")
      >> verifyPrepareProofWithClient (client session) resp
  pure resp

finalise :: ClientSession -> PrepareResp -> ClientM FinaliseResp
finalise ClientSession {client} resp = liftHandler (finaliseWithClient client resp)

runIntent :: ClientSession -> IntentW -> ClientM FinaliseResp
runIntent session intent = do
  resp <- prepareAndValidate session intent
  finalise session resp

listPending :: ClientM PendingResp
listPending = do
  ClientEnv {run} <- ask
  liftHandler (getPending run)

listClients :: ClientM ClientsResp
listClients = do
  ClientEnv {run} <- ask
  liftHandler (getClients run)

ensure :: Text -> Bool -> Either Text ()
ensure msg ok = if ok then Right () else Left msg

liftHandler :: Handler a -> ClientM a
liftHandler = ClientM . lift
