{-# LANGUAGE QuasiQuotes #-}

module WBPS.Specs.Adapter.Test (findInputsDir) where

import Path
import Path.IO

findInputsDir :: Path Abs Dir -> IO (Path Abs Dir)
findInputsDir currentDir = do
  let withinDir = currentDir </> [reldir|wbps|]
      inputsHere = currentDir </> [reldir|inputs|]
      inputsWithin = withinDir </> [reldir|inputs|]
      cabalHere = currentDir </> [relfile|wbps.cabal|]
      cabalWithin = withinDir </> [relfile|wbps.cabal|]
  hereHasInputs <- doesDirExist inputsHere
  hereHasCabal <- doesFileExist cabalHere
  if hereHasInputs && hereHasCabal
    then pure inputsHere
    else do
      withinHasInputs <- doesDirExist inputsWithin
      withinHasCabal <- doesFileExist cabalWithin
      if withinHasInputs && withinHasCabal
        then pure inputsWithin
        else
          let parentDir = parent currentDir
           in if parentDir == currentDir
                then fail "Could not locate wbps/inputs directory from current working directory."
                else findInputsDir parentDir
