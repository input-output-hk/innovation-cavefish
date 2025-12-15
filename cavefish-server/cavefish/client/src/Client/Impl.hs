{-# OPTIONS_GHC -Wno-missing-import-lists #-}

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
  startSession,
  throw422,
  eitherAs422,
  demonstrateCommitment,
  -- demonstrateCommitmentAndValidate,
  -- askSubmission,
  -- runIntent,
  listPending,
  fetchAccounts,
) where

import Cavefish.Endpoints.Read.FetchAccounts qualified as FetchAccounts
import Cavefish.Endpoints.Write.DemonstrateCommitment qualified as DemonstrateCommitment
import Client.Mock (
  Registered,
  RunServer,
  as422,
  demonstrateCommitmentWithClient,
  getPending,
  initMockClient,
  register,
 )
import Client.Mock qualified as ClientMock
import Control.Monad.Except (MonadError, liftEither, throwError)
import Control.Monad.Reader (
  MonadIO,
  MonadReader (ask),
  MonadTrans (lift),
  ReaderT (..),
 )
import Control.Monad.State (MonadState, StateT)
import Control.Monad.Trans.State (evalStateT)
import Crypto.PubKey.Ed25519 (SecretKey)
import Data.Bifunctor (first)
import Data.Map (Map)
import Data.Text (Text)
import Intent.Example.DSL (IntentDSL)
import Prototype.Messages (PendingResp)
import Servant (Handler, ServerError)
import WBPS.Core.Keys.Ed25519 qualified as Ed25519

-- | Monad for client operations against the server.
newtype ClientM a = ClientM (Control.Monad.Reader.ReaderT ClientEnv (StateT ClientState Handler) a)
  deriving newtype
    ( Control.Monad.Reader.MonadIO
    , Functor
    , Applicative
    , Monad
    , Control.Monad.Reader.MonadReader ClientEnv
    , MonadState ClientState
    , MonadError ServerError
    )

newtype ClientState = ClientState
  { littleRs :: Map Text SecretKey
  }

-- | Environment required to run client operations against the server.
data ClientEnv = ClientEnv
  { server :: RunServer
  , cardanoWalletKeyPair :: Ed25519.KeyPair
  }

-- | Represents a client session with the server.
newtype ClientSession = ClientSession
  { client :: Registered
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
  ClientEnv {server, cardanoWalletKeyPair} <- ask
  let unregistered = initMockClient server cardanoWalletKeyPair
  client <- liftHandler (register unregistered)
  pure (ClientSession client)

throw422 :: Text -> ClientM a
throw422 = throwError . as422

eitherAs422 :: Either Text a -> ClientM a
eitherAs422 = liftEither . first as422

-- | Prepare an intent with the server.
demonstrateCommitment :: ClientSession -> IntentDSL -> ClientM DemonstrateCommitment.Outputs
demonstrateCommitment ClientSession {client} intent = liftHandler (demonstrateCommitmentWithClient client intent)

-- demonstrateCommitmentAndValidate ::
--   ClientSession -> IntentDSL -> ClientM DemonstrateCommitment.Outputs
-- demonstrateCommitmentAndValidate session intent = do
--   resp <- demonstrateCommitment session intent
--   commitResp <- commit (getServer session) resp.txId
--   eitherAs422 $
--     (verifySatisfies intent resp >>= ensure "Satisfies failed")
--       >> verifyCommitProofWithClient (client session) resp commitResp
--   pure resp

-- askSubmission :: ClientSession -> DemonstrateCommitment.Outputs -> ClientM AskSubmission.Outputs
-- askSubmission ClientSession {client} resp = liftHandler (askSubmissionWithClient client resp)

-- runIntent :: ClientSession -> IntentDSL -> ClientM AskSubmission.Outputs
-- runIntent session intent = do
--   resp <- demonstrateCommitmentAndValidate session intent
--   askSubmission session resp

listPending :: ClientM PendingResp
listPending = do
  ClientEnv {server} <- ask
  liftHandler (getPending server)

fetchAccounts :: ClientM FetchAccounts.Outputs
fetchAccounts = do
  ClientEnv {server} <- ask
  liftHandler (ClientMock.fetchAccounts server)

liftHandler :: Handler a -> ClientM a
liftHandler = ClientM . lift . lift
