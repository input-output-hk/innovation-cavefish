{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module defines traceable events for the Cavefish application.
module Core.CavefishLogEvent (
  -- * Traceable Events
  HttpRoundTrip (..),
  ConfigurationLoaded (..),
  ErrorOccurred (..),
  CavefishLogEvent (..),
  RequestDuration (..),
  HttpServerError (..),
) where

import Core.Api.ServerConfiguration (ServerConfiguration)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

newtype RequestDuration = RequestDuration
  { milliseconds :: Double
  }
  deriving (Show, Generic)

instance ToJSON RequestDuration

instance FromJSON RequestDuration

-- | Represents an HTTP round trip event.
data HttpRoundTrip = HttpRoundTrip
  { path :: Text
  , method :: Text
  , duration :: RequestDuration
  }
  deriving (Show, Generic)

instance ToJSON HttpRoundTrip

instance FromJSON HttpRoundTrip

-- | Represents the event of loading the application configuration.
newtype ConfigurationLoaded = ConfigurationLoaded
  { config :: ServerConfiguration
  }
  deriving (Show, Generic)

instance ToJSON ConfigurationLoaded

instance FromJSON ConfigurationLoaded

-- | Represents an error event in the application.
newtype ErrorOccurred = ErrorOccurred
  { errorMessage :: Text
  }
  deriving (Show, Generic)

instance ToJSON ErrorOccurred

instance FromJSON ErrorOccurred

data HttpServerError = HttpServerError
  { path :: Text
  , method :: Text
  , status :: Int
  , errorMessage :: Text
  , duration :: RequestDuration
  }
  deriving (Show, Generic)

instance ToJSON HttpServerError

instance FromJSON HttpServerError

-- | Sum type representing all traceable events in the Cavefish application.
data CavefishLogEvent
  = LogHttpRoundTrip HttpRoundTrip
  | LogConfigurationLoaded ConfigurationLoaded
  | LogErrorOccurred ErrorOccurred
  | LogHttpServerError HttpServerError
  | LogSPConfigLoaded ServerConfiguration
  deriving (Show, Generic)

instance ToJSON CavefishLogEvent

instance FromJSON CavefishLogEvent
