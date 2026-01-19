module WBPS.Specs.Session.BlindSigning.ThetaStatementFixture (
  ThetaStatementFixture (..),
  loadThetaStatementFixture,
) where

import Data.String (fromString)
import Data.Text (Text)
import System.FilePath ((</>))
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Session.Demonstration.Artefacts.Commitment (Commitment)
import WBPS.Core.Session.Demonstration.Artefacts.PreparedMessage (PreparedMessage)
import WBPS.Core.Session.Demonstration.Artefacts.R (R)
import WBPS.Core.Session.Demonstration.Artefacts.Scalars (Scalars)
import WBPS.Core.Session.Demonstration.Demonstrated (
  CommitmentDemonstrated (CommitmentDemonstrated, commitment, preparedMessage, scalars),
 )
import WBPS.Core.Session.Proving.Artefacts.Challenge (Challenge)
import WBPS.Specs.Adapter.FixtureIO (assertFileExists, findRepoRoot, readJson)

data ThetaStatementFixture = ThetaStatementFixture
  { userWalletPublicKey :: UserWalletPublicKey
  , commitmentDemonstrated :: CommitmentDemonstrated
  , challenge :: Challenge
  , bigR :: R
  , expectedStatement :: [Text]
  }
  deriving (Eq, Show)

loadThetaStatementFixture :: IO ThetaStatementFixture
loadThetaStatementFixture = do
  repoRoot <- findRepoRoot
  let fixtureDir = fixtureRoot repoRoot
      statementPath = fixtureDir </> "proved/statement.json"
      challengePath = fixtureDir </> "proved/challenge.json"
      bigRPath = fixtureDir </> "proved/big_r.json"
      commitmentPath = fixtureDir </> "demonstrated/commitment.json"
      preparedMessagePath = fixtureDir </> "demonstrated/preparedMessage.json"
      scalarsPath = fixtureDir </> "demonstrated/scalars.json"
      requiredFiles =
        [ statementPath
        , challengePath
        , bigRPath
        , commitmentPath
        , preparedMessagePath
        , scalarsPath
        ]

  mapM_ assertFileExists requiredFiles
  expectedStatement <- readJson statementPath :: IO [Text]
  commitment <- readJson commitmentPath :: IO Commitment
  preparedMessage <- readJson preparedMessagePath :: IO PreparedMessage
  scalars <- readJson scalarsPath :: IO Scalars
  challenge <- readJson challengePath :: IO Challenge
  bigR <- readJson bigRPath :: IO R

  let userWalletPublicKey = fromString accountIdHex :: UserWalletPublicKey
      commitmentDemonstrated =
        CommitmentDemonstrated
          { preparedMessage
          , scalars
          , commitment
          }

  pure ThetaStatementFixture {..}

accountIdHex :: String
accountIdHex =
  "e62857e45795b0e3637ca3f525f94fd5d7c7025941016b960d4e648a2b1e6ad4"

fixtureRoot :: FilePath -> FilePath
fixtureRoot repoRoot =
  repoRoot </> "wbps/tests/integration/fixtures/theta-statement"
