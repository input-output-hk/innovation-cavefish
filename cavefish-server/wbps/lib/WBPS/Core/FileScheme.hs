{-# LANGUAGE QuasiQuotes #-}

module WBPS.Core.FileScheme (
  defaultFileScheme,
  getShellLogsFilepath,
  mkFileSchemeFromRoot,
  FileScheme (..),
  RootFolders (..),
  Accounts,
  Account,
  AccountName,
) where

import Control.Monad.RWS (MonadReader (ask))
import Data.ByteString.Lazy.Char8 qualified as BL8
import Path
import Path.IO

type Accounts = Path Abs Dir

type Account = Path Abs Dir

type AccountName = Path Rel Dir

data RootFolders = RootFolders {input :: Path Abs Dir, output :: Path Abs Dir}

mkFileSchemeFromRoot :: FilePath -> IO FileScheme
mkFileSchemeFromRoot rootPath = do
  rootDir <- resolveDir' rootPath
  pure $
    defaultFileScheme
      RootFolders
        { input = rootDir </> [reldir|inputs|]
        , output = rootDir </> [reldir|output|]
        }

defaultFileScheme :: RootFolders -> FileScheme
defaultFileScheme RootFolders {..} =
  FileScheme
    { accounts = output </> [reldir|accounts|]
    , relationCircom = input </> [reldir|relation|] </> [relfile|BuildCommitment.circom|]
    , relationR1CS = input </> [reldir|relation|] </> [relfile|relation.r1cs|]
    , buildCommitmentR1CS = input </> [reldir|relation|] </> [relfile|BuildCommitment.r1cs|]
    , buildCommitmentWASM =
        input </> [reldir|relation|] </> [reldir|BuildCommitment_js|] </> [relfile|BuildCommitment.wasm|]
    , powerOfTauPrepared = input </> [reldir|setup|] </> [relfile|powersOfTauPrepared.ptau|]
    , witnessWASM = [relfile|witness.wasm|]
    , shellLogs = [relfile|shellLogs.txt|]
    , witnessInput = [relfile|input.json|]
    , witnessOutput = [relfile|witness.wtns|]
    , proofOutput = [relfile|proof.json|]
    , statementOutput = [relfile|public.json|]
    , provingKey = [relfile|proving_key.zkey|]
    , verificationContext = [relfile|verification_context.json|]
    , accountPublicKey = [relfile|user_public_key.hex|]
    , encryptionKeys = [relfile|encryption_keys.json|]
    }

getShellLogsFilepath :: MonadReader FileScheme m => Account -> m BL8.ByteString
getShellLogsFilepath account =
  ask
    >>= \FileScheme {..} ->
      pure . BL8.pack $ Path.toFilePath (account </> shellLogs)

data FileScheme = FileScheme
  { accounts :: Accounts
  , relationCircom :: Path Abs File
  , relationR1CS :: Path Abs File
  , buildCommitmentR1CS :: Path Abs File
  , buildCommitmentWASM :: Path Abs File
  , powerOfTauPrepared :: Path Abs File
  , witnessWASM :: Path Rel File
  , shellLogs :: Path Rel File
  , witnessInput :: Path Rel File
  , witnessOutput :: Path Rel File
  , proofOutput :: Path Rel File
  , statementOutput :: Path Rel File
  , provingKey :: Path Rel File
  , verificationContext :: Path Rel File
  , accountPublicKey :: Path Rel File
  , encryptionKeys :: Path Rel File
  }
  deriving (Show, Eq)
