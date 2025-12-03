{-# LANGUAGE LambdaCase #-}

module Sp.Middleware (
  cavefishMiddleware,
  timingTraceMiddleware,
  errStatusTraceMiddleware,
) where

import Control.Monad (when)
import Core.CavefishLogEvent (
  CavefishLogEvent (LogHttpRoundTrip, LogHttpServerError),
  HttpRoundTrip (HttpRoundTrip, duration, method, path),
  HttpServerError (HttpServerError, duration, errorMessage, method, path, status),
  RequestDuration (RequestDuration),
 )
import Core.Logging (Verbosity (Verbose), traceWith, withTracer)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Network.HTTP.Types.Status (statusCode, statusMessage)
import Network.Wai (
  Middleware,
  Request,
  Response,
  rawQueryString,
  requestMethod,
  responseStatus,
 )
import System.Clock qualified as Clock

timingTraceMiddleware :: Middleware
timingTraceMiddleware app req respond =
  withTracer (Verbose "SP.Http.Roundtrip") $ \tr -> do
    begin <- getTime
    -- Call the next application in the stack
    app req $ \resp -> do
      duration <- RequestDuration . toMilliseconds . subtract begin <$> getTime
      let (path, method) = requestDetails req
          traceEvent =
            LogHttpRoundTrip $
              HttpRoundTrip
                { path
                , method
                , duration
                }
      traceWith tr traceEvent
      respond resp

errStatusTraceMiddleware :: Middleware
errStatusTraceMiddleware app req respond = do
  withTracer (Verbose "SP.Http.Erro-Status") $ \tr -> do
    begin <- getTime
    app req $ \resp -> do
      duration <- RequestDuration . toMilliseconds . subtract begin <$> getTime
      when (getStatusCode resp >= 400) $
        let (method, path) = requestDetails req
            statusCode :: Int = getStatusCode resp
            message = decodeUtf8 . statusMessage . responseStatus $ resp
         in traceWith tr $
              LogHttpServerError $
                HttpServerError
                  { path
                  , method
                  , status = statusCode
                  , errorMessage = message
                  , duration
                  }
      respond resp

getStatusCode :: Response -> Int
getStatusCode = statusCode . responseStatus

cavefishMiddleware :: Middleware
cavefishMiddleware = timingTraceMiddleware . errStatusTraceMiddleware

getTime :: IO Clock.TimeSpec
getTime = Clock.getTime Clock.Monotonic

toMilliseconds :: Clock.TimeSpec -> Double
toMilliseconds ts = fromIntegral (Clock.toNanoSecs ts) / nsPerMs

nsPerMs :: Double
nsPerMs = 1000000

requestDetails :: Request -> (Text, Text)
requestDetails req =
  ( decodeUtf8 (requestMethod req)
  , decodeUtf8 (rawQueryString req)
  )

decodeUtf8 :: ByteString -> Text
decodeUtf8 = decodeUtf8With lenientDecode
