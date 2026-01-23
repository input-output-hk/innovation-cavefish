module Main (main) where

import Core.IntentSpecs qualified as IntentSpecs
import Test.Hspec (hspec)

main :: IO ()
main = IntentSpecs.tests
