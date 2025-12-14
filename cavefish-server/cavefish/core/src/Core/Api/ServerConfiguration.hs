{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

-- | Module for loading and representing the application's configuration
module Core.Api.ServerConfiguration where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Core.Api.ServerContext (ServiceFee)
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default (def))
import Data.Text.IO qualified
import GHC.Generics (Generic)
import System.Exit (exitFailure)
import Toml (decode)
import Toml.Schema (
  FromValue (fromValue),
  GenericTomlTable (GenericTomlTable),
  Result (Failure, Success),
  ToTable,
  ToValue,
  parseTableFromValue,
  reqKey,
 )

-- | Representation of the HTTP server configuration
data HttpServer = HttpServer
  { host :: String
  , port :: Int
  }
  deriving (Show, Generic)
  deriving (ToTable, ToValue, FromValue) via GenericTomlTable HttpServer

instance ToJSON HttpServer

instance FromJSON HttpServer

instance Default HttpServer where
  def =
    HttpServer
      { host = "0.0.0.0"
      , port = 8080
      }

-- | Representation of the Weakly Blind Predicate Signature configuration
newtype Wbps = Wbps
  { path :: FilePath
  }
  deriving (Show, Generic)

instance ToJSON Wbps

instance FromJSON Wbps

instance FromValue Wbps where
  fromValue = parseTableFromValue (Wbps <$> reqKey "path")

instance Default Wbps where
  def =
    Wbps
      { path = "wbps"
      }

-- | Representation of the transaction expiry configuration
newtype TransactionExpiry = TransactionExpiry
  { seconds :: Integer
  }
  deriving (Show, Generic)

instance ToJSON TransactionExpiry

instance FromJSON TransactionExpiry

instance FromValue TransactionExpiry where
  fromValue = parseTableFromValue (TransactionExpiry <$> reqKey "seconds")

instance Default TransactionExpiry where
  def =
    TransactionExpiry
      { seconds = 3600
      }

-- | Representation of the overall application configuration
data ServerConfiguration = ServerConfiguration
  { httpServer :: HttpServer
  , wbps :: Wbps
  , serviceProviderFee :: ServiceFee
  , transactionExpiry :: TransactionExpiry
  }
  deriving (Show, Generic)
  deriving (FromValue) via GenericTomlTable ServerConfiguration

instance ToJSON ServerConfiguration

instance FromJSON ServerConfiguration

instance Default ServerConfiguration where
  def =
    ServerConfiguration
      { httpServer = def
      , wbps = def
      , serviceProviderFee = def
      , transactionExpiry = def
      }

-- | Load the configuration from a TOML file
loadConfig :: MonadIO m => FilePath -> m ServerConfiguration
loadConfig filename = do
  txt <- liftIO $ Data.Text.IO.readFile filename
  case decode txt of
    Success _ cfg -> pure cfg
    Failure err -> liftIO $ do
      putStrLn $
        "Failed to load config file: "
          <> filename
          <> "with error: "
          <> show err
      exitFailure
