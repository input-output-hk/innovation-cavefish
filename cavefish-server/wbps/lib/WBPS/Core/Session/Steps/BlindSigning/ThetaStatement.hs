-- | Utilities for rebuilding the public statement JSON expected by snarkjs.
--   The circuit exposes public inputs in the following order:
--   signer_key, commitment_point_bits, commitment_payload,
--   challenge, message_public_part.
module WBPS.Core.Session.Steps.BlindSigning.ThetaStatement (
  ThetaStatement (..),
  rebuildThetaStatement,
) where

import Data.Aeson (ToJSON (toJSON))
import Data.Default (Default (def))
import Data.Text (Text)
import WBPS.Adapter.Data.Word8 (word8sToText)
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 qualified as Ed25519
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Commitment (
  CommitmentPayload,
  commitmentPayloadToText,
 )
import WBPS.Core.Session.Steps.Demonstration.Artefacts.PreparedMessage (
  PublicMessage,
  messageBitsToText,
 )
import WBPS.Core.Session.Steps.Demonstration.Artefacts.PreparedMessage.Prepare (
  publicMessageToPublicPartBits,
 )
import WBPS.Core.Session.Steps.Demonstration.Artefacts.R (R)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.R qualified as R
import WBPS.Core.Session.Steps.Proving.Artefacts.Challenge (Challenge)
import WBPS.Core.Session.Steps.Proving.Artefacts.Challenge qualified as Challenge

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
  PublicMessage ->
  ThetaStatement
rebuildThetaStatement signerKey bigR challenge commitmentPayload publicMessage =
  let messagePublicPart = publicMessageToPublicPartBits def publicMessage
   in ThetaStatement $
        concat
          [ word8sToText (Ed25519.userWalletPublicKeyToWord8s signerKey)
          , word8sToText (R.toWord8s bigR)
          , commitmentPayloadToText commitmentPayload
          , word8sToText (Challenge.toWord8s challenge)
          , messageBitsToText messagePublicPart
          ]
