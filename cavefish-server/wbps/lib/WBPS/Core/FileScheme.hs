{-# LANGUAGE QuasiQuotes #-}
module WBPS.Core.FileScheme
    ( defaultFileScheme
    , getShellLogsFilepath
    , FileScheme(..)
    , RootFolders(..)
    , Accounts
    , Account
    , AccountName
    ) where

import Path
import Path.IO
import Control.Monad.RWS (MonadReader (ask))
import qualified Data.ByteString.Lazy.Char8 as BL8

type Accounts = Path Abs Dir
type Account = Path Abs Dir
type AccountName = Path Rel Dir

data RootFolders =  RootFolders {input :: Path Abs Dir, output :: Path Abs Dir} 


defaultFileScheme :: RootFolders -> FileScheme
defaultFileScheme RootFolders {..} =
  FileScheme
    { accounts = output </> [reldir|accounts|]
    , relationR1CS =  input </> [reldir|relation|] </> [relfile|relation.r1cs|]
    , powerOfTauPrepared = input </> [reldir|setup|] </> [relfile|powersOfTauPrepared.ptau|]
    , witnessWASM = [relfile|witness.wasm|]
    , shellLogs = [relfile|shellLogs.txt|]
    , provingKey = [relfile|proving_key.zkey|]
    , verificationKey = [relfile|verification_key.json|]
    }

getShellLogsFilepath :: MonadReader FileScheme m => Account -> m BL8.ByteString
getShellLogsFilepath  account 
    = ask 
    >>= \FileScheme{..} 
        -> pure . BL8.pack $ Path.toFilePath (account </> shellLogs)


data FileScheme = FileScheme
    { accounts :: Accounts
    , relationR1CS:: Path Abs File
    , powerOfTauPrepared :: Path Abs File
    , witnessWASM :: Path Rel File
    , shellLogs :: Path Rel File
    , provingKey :: Path Rel File
    , verificationKey :: Path Rel File} deriving (Show, Eq)
