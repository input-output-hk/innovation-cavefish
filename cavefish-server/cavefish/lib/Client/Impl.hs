{-# LANGUAGE PartialTypeSignatures #-}

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
import Control.Monad.Reader (MonadIO (liftIO), MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.State (MonadState (..), StateT, modify)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (evalStateT)
import Core.Intent (IntentW)
import Crypto.Error (CryptoFailable (..))
import Crypto.PubKey.Ed25519 (PublicKey, SecretKey)
import Crypto.PubKey.Ed25519 qualified as Ed
import Crypto.Random (MonadRandom (..))
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Servant (Handler, ServerError)
import Sp.Server (ClientsResp, FinaliseResp, PendingResp, PrepareResp)

newtype ClientM a = ClientM (ReaderT ClientEnv (StateT ClientState Handler) a)
  deriving newtype
    (Functor, Applicative, Monad, MonadReader ClientEnv, MonadState ClientState, MonadError ServerError)

newtype ClientState = ClientState
  { littleRs :: Map Text SecretKey
  }

data ClientEnv = ClientEnv
  { run :: RunServer
  , lcSk :: SecretKey
  , spPk :: PublicKey
  }

newtype ClientSession = ClientSession
  { client :: MockClient
  }

runClient :: ClientEnv -> ClientM a -> Handler a
runClient env (ClientM m) = do
  flip evalStateT (ClientState mempty) $ runReaderT m env

withSession :: ClientEnv -> (ClientSession -> ClientM a) -> Handler a
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

prepare :: ClientSession -> IntentW -> ClientM PrepareResp
prepare ClientSession {client} intent = liftHandler (prepareWithClient client intent)

prepareAndValidate :: ClientSession -> IntentW -> ClientM PrepareResp
prepareAndValidate session intent = do
  resp <- prepare session intent
  eitherAs422 $
    (verifySatisfies intent resp >>= ensure "Satisfies failed")
      >> verifyPrepareProofWithClient (client session) resp
  pure resp

commit :: Text -> ClientM Ed.PublicKey
commit txId = do
  randomBytes :: ByteString <- liftHandler $ liftIO $ getRandomBytes 32
  r <- case Ed.secretKey randomBytes of
    CryptoPassed c -> pure c
    CryptoFailed e -> throwError (as422 "TODO")
  modify (\ClientState {littleRs} -> ClientState $ Map.insert txId r littleRs)
  let bigR = Ed.toPublic r
  pure bigR

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
