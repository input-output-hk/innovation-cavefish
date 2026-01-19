module WBPS.Specs.Session.BlindSigning.ThetaStatement (specs) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import WBPS.Core.Session.BlindSigning.ThetaStatement (
  ThetaStatement (ThetaStatement),
  rebuildThetaStatement,
 )
import WBPS.Core.Session.Demonstration.Artefacts.Commitment (
  Commitment (Commitment, payload),
 )
import WBPS.Core.Session.Demonstration.Artefacts.PreparedMessage (
  MessageParts (MessageParts, public),
  PreparedMessage (PreparedMessage, parts),
 )
import WBPS.Core.Session.Demonstration.Demonstrated (
  CommitmentDemonstrated (CommitmentDemonstrated, commitment, preparedMessage),
 )
import WBPS.Specs.Session.BlindSigning.ThetaStatementFixture (
  ThetaStatementFixture (ThetaStatementFixture, bigR, challenge, commitmentDemonstrated, expectedStatement, userWalletPublicKey),
  loadThetaStatementFixture,
 )

specs :: TestTree
specs =
  testGroup
    "BlindSigning"
    [ testCase "rebuilds statement.json from output artefacts" rebuildsStatement
    ]

rebuildsStatement :: Assertion
rebuildsStatement = do
  ThetaStatementFixture {userWalletPublicKey, commitmentDemonstrated, challenge, bigR, expectedStatement} <-
    loadThetaStatementFixture

  let CommitmentDemonstrated
        { commitment = Commitment {payload = commitmentPayload}
        , preparedMessage = PreparedMessage {parts = MessageParts {public = publicMessage}}
        } = commitmentDemonstrated
      ThetaStatement actual =
        rebuildThetaStatement userWalletPublicKey bigR challenge commitmentPayload publicMessage

  actual @?= expectedStatement
