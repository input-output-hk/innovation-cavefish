{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A simple tracer implementation that logs messages as JSON to stdout,
-- with support for verbosity levels and capturing logs on failure.
module Adapter.Logging (
  -- * Tracer
  Tracer (..),
  natTracer,
  nullTracer,
  traceWith,

  -- * Using it
  Verbosity (..),
  Envelope (..),
  withTracer,
  withTracerOutputTo,
  showLogsOnFailure,
  traceInTVar,
  contramap,
) where

import Control.Concurrent (myThreadId)
import Control.Concurrent.Async (withAsync)
import Control.Concurrent.STM.TBQueue (
  flushTBQueue,
  newTBQueueIO,
  readTBQueue,
  writeTBQueue,
 )
import Control.Concurrent.STM.TVar (
  TVar,
  modifyTVar,
  newTVarIO,
  readTVarIO,
 )
import Control.Monad (forM_, forever, (>=>))
import Control.Monad.Catch (
  MonadCatch,
  finally,
  onException,
 )
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Time (MonadTime, currentTime)
import Control.Tracer (
  Tracer (Tracer, runTracer),
  contramap,
  natTracer,
  nullTracer,
  traceWith,
 )
import Data.Aeson (FromJSON, ToJSON, pairs, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Time (UTCTime)
import GHC.Conc (atomically)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Say (say)
import System.IO (Handle, hFlush, stdout)
import Text.Read (readMaybe)

data Verbosity = Quiet | Verbose Text
  deriving (Eq, Show, Generic)

instance ToJSON Verbosity

instance FromJSON Verbosity

-- | Provides logging metadata for entries.
data Envelope a = Envelope
  { timestamp :: UTCTime
  , threadId :: Int
  , namespace :: Text
  , message :: a
  }
  deriving (Eq, Show, Generic)

instance FromJSON a => FromJSON (Envelope a)

instance ToJSON a => ToJSON (Envelope a) where
  toEncoding Envelope {timestamp, threadId, namespace, message} =
    pairs $
      mconcat
        [ "timestamp" .= timestamp
        , "threadId" .= threadId
        , "namespace" .= namespace
        , "message" .= message
        ]

defaultQueueSize :: Natural
defaultQueueSize = 500

-- | Start logging thread and acquire a 'Tracer'. This tracer will dump all
-- messsages on @stdout@, one message per line, formatted as JSON. This tracer
-- is wrapping 'msg' into an 'Envelope' with metadata.
withTracer ::
  forall m msg a.
  (MonadIO m, MonadTime m, ToJSON msg) =>
  Verbosity ->
  (Tracer m msg -> IO a) ->
  IO a
withTracer Quiet = ($ nullTracer)
withTracer (Verbose namespace) = withTracerOutputTo stdout namespace

-- | Start logging thread acquiring a 'Tracer', outputting JSON formatted
-- messages to some 'Handle'. This tracer is wrapping 'msg' into an 'Envelope'
-- with metadata.
withTracerOutputTo ::
  forall m msg a.
  (MonadIO m, MonadTime m, ToJSON msg) =>
  Handle ->
  Text ->
  (Tracer m msg -> IO a) ->
  IO a
withTracerOutputTo hdl namespace action = do
  msgQueue <- liftIO (newTBQueueIO @(Envelope msg) defaultQueueSize)
  withAsync (writeLogs msgQueue) $ \_ ->
    action (tracer msgQueue) `finally` flushLogs msgQueue
  where
    tracer queue =
      tbqTracer $
        mkEnvelope namespace >=> liftIO . atomically . writeTBQueue queue

    writeLogs queue =
      forever $ do
        atomically (readTBQueue queue) >>= write . Aeson.encode
        hFlush hdl

    flushLogs queue = liftIO $ do
      entries <- atomically $ flushTBQueue queue
      forM_ entries (write . Aeson.encode)
      hFlush hdl

    write bs = LBS.hPut hdl (bs <> "\n")

tbqTracer :: (msg -> m ()) -> Tracer m msg
tbqTracer action = Tracer $ \msg -> action msg

-- | Capture logs and output them to stdout when an exception was raised by the
-- given 'action'. This tracer is wrapping 'msg' into an 'Envelope' with
-- metadata.
showLogsOnFailure ::
  (MonadCatch m, MonadIO m, MonadTime m, ToJSON msg) =>
  (Tracer m msg -> m a) ->
  m a
showLogsOnFailure action = do
  tvar <- liftIO (newTVarIO [])
  action (traceInTVar tvar)
    `onException` ( liftIO (readTVarIO tvar)
                      >>= mapM_ (say . TL.toStrict . decodeUtf8 . Aeson.encode) . reverse
                  )

traceInTVar ::
  (MonadIO m, MonadTime m) =>
  TVar [Envelope msg] ->
  Tracer m msg
traceInTVar tvar = tbqTracer $ \msg -> do
  envelope <- mkEnvelope "" msg
  liftIO $ atomically $ modifyTVar tvar (envelope :)

-- * Internal functions

mkEnvelope :: (MonadTime m, MonadIO m) => Text -> msg -> m (Envelope msg)
mkEnvelope namespace message = do
  timestamp <- currentTime
  -- threadId <- mkThreadId <$> (liftIO myThreadId)
  threadId <- mkThreadId <$> (liftIO myThreadId)
  pure $ Envelope {namespace, timestamp, threadId, message}
  where
    -- NOTE(AB): This is a bit contrived but we want a numeric threadId and we
    -- get some text which we know the structure of
    mkThreadId = fromMaybe 0 . readMaybe . Text.unpack . Text.drop 9 . Text.pack . show
