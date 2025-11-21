module Main (main) where

import Path.IO (getCurrentDir, getTempDir, withTempDir)
import Test.Tasty (defaultMain, localOption, testGroup)
import Test.Tasty.QuickCheck (QuickCheckTests (..))
import Test.Tasty.Runners (NumThreads (..))
import WBPS
import WBPS.Core.FileScheme (RootFolders (..))
import WBPS.Specs.Adapter.Test (findInputsDir)
import WBPS.Specs.NominalCase qualified as Register.Nominal.Case

main :: IO ()
main = do
  inputFolder <- findInputsDir =<< getCurrentDir
  tempRoot <- getTempDir
  withTempDir tempRoot "wbps-integration" $ \outputFolder ->
    defaultMain $
      testGroup
        "[WBPS - integration specs]"
        [ testGroup
            "Nominal Cases"
            [ localOption (NumThreads 8) . localOption (QuickCheckTests 10) $
                Register.Nominal.Case.specs RootFolders {input = inputFolder, output = outputFolder}
            ]
        ]
