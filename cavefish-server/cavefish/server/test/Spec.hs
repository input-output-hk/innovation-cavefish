module Spec (spec) where

import Core.IntentSpecs qualified as IntentSpecs
import Test.Hspec (Spec)

spec :: Spec
spec = IntentSpecs.spec
