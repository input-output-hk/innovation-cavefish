module Spec (spec) where

import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "cavefish-core specs" $ do
    it "tests are under development" $ do
      True `shouldBe` True
