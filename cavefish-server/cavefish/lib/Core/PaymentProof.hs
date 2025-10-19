{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.PaymentProof (
  ProofResult (..),
  mkPaymentProof,
  verifyPaymentProof,
  hashTxAbs,
) where

import Cardano.Api (ConwayEra, Tx)
import Cardano.Api qualified as Api
import Core.Cbor (serialiseTxAbs)
import Core.Intent (Intent)
import Core.Pke (PkeCiphertext, ciphertextDigest)
import Core.Proof (Proof, mkProof, verifyProof)
import Core.TxAbs (TxAbs)
import Crypto.Hash (SHA256 (..), hash)
import Crypto.PubKey.Ed25519 (PublicKey, SecretKey)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, (.:), (.=))
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

-- | Minimal proof wrapper so we can swap in a real zk proof later.
data ProofResult
  = ProofEd25519 Proof
  | ProofGroth16 Proof
  | ProofStub
  deriving (Eq, Show, Generic)

instance ToJSON ProofResult where
  toJSON = \case
    ProofEd25519 proof ->
      object
        [ "kind" .= String "ed25519"
        , "proof" .= proof
        ]
    ProofGroth16 proof ->
      object
        [ "kind" .= String "groth16"
        , "proof" .= proof
        ]
    ProofStub ->
      object ["kind" .= String "stub"]

instance FromJSON ProofResult where
  parseJSON = withObject "ProofResult" $ \o -> do
    kind <- o .: "kind"
    case (kind :: Text) of
      "ed25519" -> ProofEd25519 <$> o .: "proof"
      -- TODO WG: Obviously placeholder
      "groth16" -> ProofEd25519 <$> o .: "proof"
      "stub" -> pure ProofStub
      other -> fail ("unknown proof kind: " <> T.unpack other)

-- TODO WG: Placeholder
mkPaymentProof ::
  SecretKey ->
  Intent ->
  Tx Api.ConwayEra ->
  TxAbs ConwayEra ->
  PkeCiphertext ->
  ByteString ->
  ByteString ->
  IO ProofResult
mkPaymentProof secretKey _intent tx txAbs ciphertext _auxNonce _rho = do
  let txId = Api.getTxId (Api.getTxBody tx)
      txAbsHash = hashTxAbs txAbs
      commitmentBytes = ciphertextDigest ciphertext
      proof = mkProof secretKey txId txAbsHash commitmentBytes
  pure (ProofEd25519 proof)

-- TODO WG: Placeholder
verifyPaymentProof ::
  PublicKey ->
  ProofResult ->
  TxAbs ConwayEra ->
  Api.TxId ->
  PkeCiphertext ->
  ByteString ->
  Either Text ()
verifyPaymentProof _ ProofStub _ _ _ _ = Right ()
verifyPaymentProof _ (ProofGroth16 _) _ _ _ _ = Right ()
verifyPaymentProof publicKey (ProofEd25519 proof) txAbs txId ciphertext _auxNonce =
  let commitmentBytes = ciphertextDigest ciphertext
   in if verifyProof publicKey txId (hashTxAbs txAbs) commitmentBytes proof
        then Right ()
        else Left "ed25519 proof verification failed"

hashTxAbs :: TxAbs Api.ConwayEra -> ByteString
hashTxAbs = BA.convert . (hash @_ @SHA256) . serialiseTxAbs
