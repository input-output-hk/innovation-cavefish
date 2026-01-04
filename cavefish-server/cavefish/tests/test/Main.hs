module Main (main) where

import Cavefish.Nominal qualified
import Test.Hspec (hspec)

main :: IO ()
main = hspec Cavefish.Nominal.spec
