{-# LANGUAGE QuasiQuotes #-}

module WBPS.Core.FileScheme (
  defaultFileScheme,
  getShellLogsFilepath,
  mkFileSchemeFromRoot,
  FileScheme (..),
  Setup (..),
  Session (..),
  BuildCommitmentSetup (..),
  BuildCommitment (..),
  WitnessGeneration (..),
  WitnessGenerationSetup (..),
  Account (..),
  RootFolders (..),
) where

import Control.Monad.RWS (MonadReader (ask))
import Data.ByteString.Lazy.Char8 qualified as BL8
import Path (
  Abs,
  Dir,
  File,
  Path,
  Rel,
  parseAbsDir,
  reldir,
  relfile,
  toFilePath,
  (</>),
 )
import Path.IO (ensureDir)
import System.Environment (getEnv)
import WBPS.Core.Registration.FileScheme.Directories qualified as Directory

data RootFolders = RootFolders {input :: Path Abs Dir, output :: Path Abs Dir}

mkFileSchemeFromRoot :: Path Rel Dir -> IO FileScheme
mkFileSchemeFromRoot dirLabel = do
  inputRoot <- requiredEnvDir "WBPS_TEST_INPUT_ROOT"
  outputRoot <- requiredEnvDir "WBPS_TEST_OUTPUT_ROOT"
  ensureDir $ outputRoot </> dirLabel
  pure $
    defaultFileScheme
      RootFolders
        { input = inputRoot
        , output = outputRoot </> dirLabel
        }

requiredEnvDir :: String -> IO (Path Abs Dir)
requiredEnvDir envVar = parseAbsDir =<< getEnv envVar

defaultFileScheme :: RootFolders -> FileScheme
defaultFileScheme RootFolders {..} =
  FileScheme
    { setup =
        Setup
          { relationR1CS = input </> [reldir|relation|] </> [relfile|relation.r1cs|]
          , powerOfTauPrepared = input </> [relfile|powersOfTauPrepared.ptau|]
          , commitment =
              BuildCommitmentSetup
                { circom = input </> [reldir|commitment|] </> [relfile|BuildCommitment.circom|]
                , r1cs = input </> [reldir|commitment|] </> [relfile|BuildCommitment.r1cs|]
                , wasm = input </> [reldir|commitment|] </> [reldir|BuildCommitment_js|] </> [relfile|BuildCommitment.wasm|]
                }
          , witness =
              WitnessGenerationSetup
                { wasm = input </> [reldir|witness|] </> [relfile|generator.wasm|]
                }
          }
    , accounts = output </> [reldir|accounts|]
    , account =
        Account
          { provingKey = [relfile|proving_key.zkey|]
          , verificationContext = [relfile|verification_context.json|]
          , userPublicKey = [relfile|user_public_key.hex|]
          , encryptionKeys = [relfile|encryption_keys.json|]
          , sessions = [reldir|sessions|]
          , session =
              Session
                { message = [relfile|message.json|]
                , rho = [relfile|rho.json|]
                , commitment =
                    BuildCommitment
                      { scalars = [relfile|scalars.json|]
                      , commitment = [relfile|commitment.json|]
                      , input = [relfile|internal_input.json|]
                      , output = [relfile|internal_witness.wtns|]
                      , statementOutput = [relfile|internal_public.json|]
                      }
                , witness =
                    WitnessGeneration
                      { input = [relfile|witness_input.json|]
                      , output = [relfile|witness.wtns|]
                      }
                , proof =
                    ProofGeneration
                      { statement = [relfile|statement.json|]
                      , proof = [relfile|proof.json|]
                      }
                }
          , shellLogs = [relfile|shellLogs.txt|]
          }
    }

getShellLogsFilepath :: MonadReader FileScheme m => Directory.Account -> m BL8.ByteString
getShellLogsFilepath accountDirectory =
  ask
    >>= \FileScheme {account = Account {shellLogs}} ->
      pure . BL8.pack $ Path.toFilePath (accountDirectory </> shellLogs)

data FileScheme = FileScheme
  { setup :: Setup
  , accounts :: Directory.Accounts
  , account :: Account
  }
  deriving (Show, Eq)

data Setup = Setup
  { relationR1CS :: Path Abs File
  , powerOfTauPrepared :: Path Abs File
  , commitment :: BuildCommitmentSetup
  , witness :: WitnessGenerationSetup
  }
  deriving (Show, Eq)

newtype WitnessGenerationSetup
  = WitnessGenerationSetup
  { wasm :: Path Abs File
  }
  deriving (Show, Eq)

data BuildCommitmentSetup
  = BuildCommitmentSetup
  { circom :: Path Abs File
  , r1cs :: Path Abs File
  , wasm :: Path Abs File
  }
  deriving (Show, Eq)

type Sessions = Path Rel Dir

data Account = Account
  { userPublicKey :: Path Rel File
  , encryptionKeys :: Path Rel File
  , provingKey :: Path Rel File
  , verificationContext :: Path Rel File
  , sessions :: Sessions
  , session :: Session
  , shellLogs :: Path Rel File
  }
  deriving (Show, Eq)

data Session = Session
  { message :: Path Rel File
  , rho :: Path Rel File
  , commitment :: BuildCommitment
  , witness :: WitnessGeneration
  , proof :: ProofGeneration
  }
  deriving (Show, Eq)

data WitnessGeneration
  = WitnessGeneration
  { input :: Path Rel File
  , output :: Path Rel File
  }
  deriving (Show, Eq)

data ProofGeneration
  = ProofGeneration
  { statement :: Path Rel File
  , proof :: Path Rel File
  }
  deriving (Show, Eq)

data BuildCommitment
  = BuildCommitment
  { input :: Path Rel File
  , output :: Path Rel File
  , statementOutput :: Path Rel File
  , commitment :: Path Rel File
  , scalars :: Path Rel File
  }
  deriving (Show, Eq)
