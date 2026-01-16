-- | Utilities for rebuilding the public statement JSON expected by snarkjs.
--   The circuit exposes public inputs in the following order:
--   signer_key, commitment_point_bits, commitment_point_affine, commitment_payload,
--   challenge, message_public_part.
module WBPS.Core.Session.BlindSigning.ThetaStatement (
  ThetaStatement (..),
  rebuildThetaStatement,
  rebuildThetaStatementFromDemonstrated,
) where

import Data.Aeson (ToJSON (toJSON))
import Data.Text (Text)
import WBPS.Adapter.Data.Word8 (word8sToText)
import WBPS.Adapter.Math.AffinePoint (AffinePoint)
import WBPS.Adapter.Math.AffinePoint qualified as AffinePoint
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 qualified as Ed25519
import WBPS.Core.Session.Demonstration.Artefacts.Commitment (
  CommitmentPayload,
  commitmentPayloadToText,
  payload,
 )
import WBPS.Core.Session.Demonstration.Artefacts.PreparedMessage (
  CircuitMessage (CircuitMessage, public),
  MessageBits,
  PreparedMessage (PreparedMessage, circuit),
  messageBitsToText,
 )
import WBPS.Core.Session.Demonstration.Artefacts.R (R)
import WBPS.Core.Session.Demonstration.Artefacts.R qualified as R
import WBPS.Core.Session.Demonstration.Artefacts.Scalars (gPowRho)
import WBPS.Core.Session.Demonstration.Demonstrated (
  CommitmentDemonstrated (CommitmentDemonstrated, commitment, preparedMessage, scalars),
 )
import WBPS.Core.Session.Proving.Artefacts.Challenge (Challenge)
import WBPS.Core.Session.Proving.Artefacts.Challenge qualified as Challenge

newtype ThetaStatement = ThetaStatement {unThetaStatement :: [Text]}
  deriving (Eq, Show)

instance ToJSON ThetaStatement where
  toJSON (ThetaStatement values) = toJSON values

-- | Rebuild the statement.json payload from the public artefacts.
rebuildThetaStatement ::
  UserWalletPublicKey ->
  R ->
  Challenge ->
  CommitmentPayload ->
  AffinePoint ->
  MessageBits ->
  ThetaStatement
rebuildThetaStatement signerKey bigR challenge commitmentPayload commitmentPointAffine messagePublicPart =
  ThetaStatement $
    concat
      [ word8sToText (Ed25519.userWalletPublicKeyToWord8s signerKey)
      , word8sToText (R.toWord8s bigR)
      , AffinePoint.toText commitmentPointAffine
      , commitmentPayloadToText commitmentPayload
      , word8sToText (Challenge.toWord8s challenge)
      , messageBitsToText messagePublicPart
      ]

-- | Convenience wrapper for values produced during demonstration/proving.
rebuildThetaStatementFromDemonstrated ::
  UserWalletPublicKey ->
  R ->
  Challenge ->
  CommitmentDemonstrated ->
  ThetaStatement
rebuildThetaStatementFromDemonstrated signerKey bigR challenge CommitmentDemonstrated {commitment, preparedMessage, scalars} =
  rebuildThetaStatement
    signerKey
    bigR
    challenge
    (payload commitment)
    (gPowRho scalars)
    messagePublicPart
  where
    PreparedMessage {circuit = CircuitMessage {public = messagePublicPart}} = preparedMessage
