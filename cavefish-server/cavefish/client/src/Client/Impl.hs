-- | Client implementation using a mock client to interact with the server.
--
--  This module defines the `ClientM` monad and associated functions to perform
--  client operations such as preparing intents, validating proofs, finalizing
--  transactions, and listing pending requests and registered clients.
module Client.Impl (
  ClientEnv (..),
  ClientState (..),
  ClientSession (..),
  ClientM,
  runClient,
  withSession,
  commit,
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
  MockClient (mcRun),
  RunServer,
  as422,
  finaliseWithClient,
  getClients,
  getPending,
  initMockClient,
  prepareWithClient,
  register,
  runCommit,
  verifyCommitProofWithClient,
  verifySatisfies,
 )
import Control.Monad.Except (MonadError, liftEither, throwError)
import Control.Monad.Reader (MonadIO (liftIO), MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.State (MonadState, StateT, modify)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (evalStateT)
import Core.Api.Messages (ClientsResp, CommitResp, FinaliseResp, PendingResp, PrepareResp (txId))
import Core.Intent (IntentW)
import Crypto.Error (CryptoFailable (CryptoFailed, CryptoPassed))
import Crypto.PubKey.Ed25519 (SecretKey)
import Crypto.PubKey.Ed25519 qualified as Ed
import Crypto.Random (MonadRandom (getRandomBytes))
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Servant (Handler, ServerError)

-- | Monad for client operations against the server.
newtype ClientM a = ClientM (ReaderT ClientEnv Handler a)
  deriving newtype (Functor, Applicative, Monad, MonadReader ClientEnv, MonadError ServerError)

-- | Environment required to run client operations against the server.
data ClientEnv = ClientEnv
  { run :: RunServer
  , lcSk :: SecretKey
  }

-- | Represents a client session with the server.
newtype ClientSession = ClientSession
  { client :: MockClient
  }

-- | Run a ClientM action with the given ClientEnv.
runClient :: ClientEnv -> ClientM a -> Handler a
runClient env (ClientM m) = do
  flip evalStateT (ClientState mempty) $ runReaderT m env

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
  ClientEnv {run, lcSk} <- ask
  let unregistered = initMockClient run lcSk
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
  commitResp <- commit (mcRun session.client) resp.txId
  eitherAs422 $
    (verifySatisfies intent resp >>= ensure "Satisfies failed")
      >> verifyCommitProofWithClient (client session) resp commitResp
  pure resp

commit :: RunServer -> Text -> ClientM CommitResp
commit run txId = do
  randomBytes :: ByteString <- liftHandler $ liftIO $ getRandomBytes 32
  r <- case Ed.secretKey randomBytes of
    CryptoPassed c -> pure c
    CryptoFailed _ -> throwError (as422 "couldn't generate secret key during commit")
  modify (\ClientState {littleRs} -> ClientState $ Map.insert txId r littleRs)
  let bigR = Ed.toPublic r
  liftHandler (runCommit run txId bigR)

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
liftHandler = ClientM . lift . lift
