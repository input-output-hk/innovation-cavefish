
module Main (main) where

import Path.IO (getCurrentDir, getTempDir, withTempDir)
import Test.Tasty (defaultMain, testGroup, localOption)
import Test.Tasty.QuickCheck (QuickCheckTests (..))
import Test.Tasty.Runners (NumThreads (..))

import qualified WBPS.Specs.NominalCase as Register.Nominal.Case
import WBPS.Specs.Adapter.Test (findInputsDir)

import WBPS
import WBPS.Core.FileScheme (RootFolders(..))

main :: IO ()
main = do
    inputFolder <- findInputsDir =<< getCurrentDir
    tempRoot <- getTempDir
    withTempDir tempRoot "wbps-integration" $ \outputFolder ->
        defaultMain $
            testGroup
                "[WBPS - integration specs]"
                [ testGroup "Nominal Cases"
                    [ localOption (NumThreads 4) . localOption (QuickCheckTests 20)
                      $ Register.Nominal.Case.specs RootFolders {input = inputFolder, output = outputFolder}
                    ]
                ]
