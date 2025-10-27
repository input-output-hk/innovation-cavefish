module Main (main) where

import Spec qualified
import Test.Hspec (hspec)

main :: IO ()
main = hspec Spec.spec
