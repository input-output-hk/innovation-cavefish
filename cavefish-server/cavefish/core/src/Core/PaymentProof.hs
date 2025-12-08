{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.PaymentProof (
  ProofResult (..),
  mkPaymentProof,
  verifyPaymentProof,
  hashTxAbs,
) where

import Cardano.Api (ConwayEra, Tx, TxId, getTxBody, getTxId)
import Core.Cbor (serialiseTxAbs)
import Core.Intent (CanonicalIntent)
import Core.Pke (PkeCiphertext, ciphertextDigest)
import Core.Proof (Proof, mkProof)
import Core.TxAbs (TxAbs)
import Crypto.Hash (SHA256, hash)
import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toJSON),
  Value (String),
  object,
  withObject,
  (.:),
  (.=),
 )
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import WBPS.Core.Keys.Ed25519 qualified as Ed25519

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
{- This will end up being split into a multi-step process, described in figure 3
   of the paper:

   Signer (LC)                     Service Provider (SP)
  ----------------------------------------------------------------
  WBPS Execution for m := tx||auxnt        Produce commitment comtx
                                (comtx, TxAbs)
                                <-----------
  Produce blind sig. com. `R = g^r`
                                      R
                                ----------->
                                           Produce challenge `c` and proof `π`
                                    (c, π)
                                <-----------
          Check proof `π`
                                      s
          Produce `s = r + cx`  -----------> Produce signature `σ = (R, s)`
  -}
mkPaymentProof ::
  Ed25519.PrivateKey ->
  CanonicalIntent ->
  Tx ConwayEra ->
  TxAbs ConwayEra ->
  PkeCiphertext ->
  ByteString ->
  ByteString ->
  IO ProofResult
mkPaymentProof secretKey _intent tx txAbs ciphertext _auxNonce _rho = do
  let txId = getTxId (getTxBody tx)
      txAbsHash = hashTxAbs txAbs
      commitmentBytes = ciphertextDigest ciphertext
      proof = mkProof secretKey txId txAbsHash commitmentBytes
  pure (ProofEd25519 proof)

-- TODO WG: Placeholder
verifyPaymentProof ::
  Ed25519.PublicKey ->
  ProofResult ->
  TxAbs ConwayEra ->
  TxId ->
  PkeCiphertext ->
  ByteString ->
  Either Text ()
verifyPaymentProof _ ProofStub _ _ _ _ = Right ()
verifyPaymentProof _ (ProofGroth16 _) _ _ _ _ = Right ()
verifyPaymentProof _ (ProofEd25519 _) _ _ _ _auxNonce = Right ()

-- verifyPaymentProof publicKey (ProofEd25519 proof) txAbs txId ciphertext _auxNonce = Right ()
-- let commitmentBytes = ciphertextDigest ciphertext
--  in if verifyProof publicKey txId (hashTxAbs txAbs) commitmentBytes proof
--       then Right ()
--       else Left "ed25519 proof verification failed"

hashTxAbs :: TxAbs ConwayEra -> ByteString
hashTxAbs = BA.convert . (hash @_ @SHA256) . serialiseTxAbs
