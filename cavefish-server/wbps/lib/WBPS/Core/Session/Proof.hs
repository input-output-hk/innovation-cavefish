{-# LANGUAGE DerivingStrategies #-}

module WBPS.Core.Session.Proof (
  Proof (..),
) where

import Data.Aeson.Types (FromJSON, ToJSON, Value)
import GHC.Generics (Generic)

newtype Proof
  = Proof Value
  deriving stock (Eq, Show, Generic)
  deriving newtype (FromJSON, ToJSON)
