{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module WBPS.Core.Primitives.Circom (

) where

import Control.Monad.Trans.Maybe
import Data.Bool (bool)
import Data.ByteString
import Path
import Path.IO
import Shh

load
  SearchPath
  [ "echo"
  , "circom"
  ]

-- compile
