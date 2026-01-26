{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson (FromJSON, eitherDecode, encode)
import qualified Data.ByteString.Lazy as BL
import Data.String (fromString)
import System.FilePath ((</>))
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Session.Steps.BlindSigning.ThetaStatement (ThetaStatement (ThetaStatement), rebuildThetaStatement)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Commitment (Commitment (Commitment, payload))
import WBPS.Core.Session.Steps.Demonstration.Artefacts.PreparedMessage (MessageParts (MessageParts, public), PreparedMessage (PreparedMessage, parts))
import WBPS.Core.Session.Steps.Demonstration.Artefacts.R (R)
import WBPS.Core.Session.Steps.Proving.Artefacts.Challenge (Challenge)

main :: IO ()
main = do
  let fixtureDir = "wbps/tests/integration/fixtures/theta-statement"
      statementPath = fixtureDir </> "proved/statement.json"
      challengePath = fixtureDir </> "proved/challenge.json"
      bigRPath = fixtureDir </> "proved/big_r.json"
      commitmentPath = fixtureDir </> "demonstrated/commitment.json"
      preparedMessagePath = fixtureDir </> "demonstrated/preparedMessage.json"

  commitment <- readJson commitmentPath
  preparedMessage <- readJson preparedMessagePath
  challenge <- readJson challengePath
  bigR <- readJson bigRPath

  let userWalletPublicKey = fromString accountIdHex :: UserWalletPublicKey
      Commitment {payload = commitmentPayload} = commitment
      PreparedMessage {parts = MessageParts {public = publicMessage}} = preparedMessage
      ThetaStatement statement =
        rebuildThetaStatement userWalletPublicKey bigR challenge commitmentPayload publicMessage

  BL.writeFile statementPath (encode statement)

readJson :: FromJSON a => FilePath -> IO a
readJson path = do
  bytes <- BL.readFile path
  case eitherDecode bytes of
    Right value -> pure value
    Left err -> fail ("Failed to decode fixture " <> path <> ": " <> err)

accountIdHex :: String
accountIdHex =
  "e62857e45795b0e3637ca3f525f94fd5d7c7025941016b960d4e648a2b1e6ad4"
