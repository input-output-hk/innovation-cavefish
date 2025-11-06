{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE QuasiQuotes #-}


module WBPS.Core.Primitives.Circom 
    ( 
    ) where

import Shh
import Path
import Path.IO
import Data.ByteString
import Data.Bool (bool)
import Control.Monad.Trans.Maybe

load SearchPath
    [ "echo"
    , "circom"
    ]


-- compile 