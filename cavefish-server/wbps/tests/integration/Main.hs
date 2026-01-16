module Main (main) where

import Test.Tasty (defaultMain, localOption, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests (QuickCheckTests))
import Test.Tasty.Runners (NumThreads (NumThreads))
import WBPS.Specs.NominalCase qualified as Register.Nominal.Case
import WBPS.Specs.Session.BlindSigning.ThetaStatement qualified as BlindSigning.ThetaStatement
import WBPS.Specs.Session.Demonstration.Commitment.Build qualified as Commitment.BuildCommitment

main :: IO ()
main =
  defaultMain $
    testGroup
      "[WBPS - integration specs]"
      [ testGroup
          "Nominal Cases"
          [ localOption (NumThreads 8) . localOption (QuickCheckTests 4) $ Register.Nominal.Case.specs
          , Commitment.BuildCommitment.specs
          , BlindSigning.ThetaStatement.specs
          ]
      ]
