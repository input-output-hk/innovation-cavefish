{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ClientBackend.App
  ( Env (..)
  , AppM (..)
  , runApp
  ) where

import Control.Monad.Error.Class (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Servant.Server (Handler, ServerError)

data Env = Env

newtype AppM a = AppM {unAppM :: ReaderT Env Handler a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader Env, MonadError ServerError)

runApp :: Env -> AppM a -> Handler a
runApp env (AppM m) = runReaderT m env
