{-# LANGUAGE OverloadedStrings #-}

module Core.Pke (
  PkeSecretKey (..),
  PkePublicKey (..),
  PkeCiphertext (..),
  CommitmentSeeds (..),
  PkeError (..),
  generateKeyPair,
  deriveSecretKey,
  toPublicKey,
  encrypt,
  encryptWithSeeds,
  deriveCommitmentSeeds,
  decrypt,
  serialiseCiphertext,
  ciphertextDigest,
  renderError,
  deserialiseCiphertext,
) where

import Codec.CBOR.Encoding qualified as E
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Term (Term (TBytes, TList), decodeTerm)
import Codec.CBOR.Write qualified as Write
import Crypto.Cipher.ChaChaPoly1305 qualified as ChaCha
import Crypto.Error (CryptoFailable (CryptoFailed, CryptoPassed))
import Crypto.Hash (SHA512, hash)
import Crypto.PubKey.Curve25519 qualified as Curve25519
import Data.Bifunctor (first)
import Data.ByteArray qualified as BA
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T

newtype PkeSecretKey = PkeSecretKey {unSecretKey :: Curve25519.SecretKey}
  deriving (Eq, Show)

newtype PkePublicKey = PkePublicKey {unPublicKey :: Curve25519.PublicKey}
  deriving (Eq, Show)

data PkeCiphertext = PkeCiphertext
  { ephemeralPublic :: ByteString
  , nonce :: ByteString
  , payload :: ByteString
  , authTag :: ByteString
  }
  deriving (Eq, Show)

data CommitmentSeeds = CommitmentSeeds
  { seedX :: Integer
  , seedY :: Integer
  }
  deriving (Eq, Show)

data PkeError
  = PkeInvalidSeed
  | PkeCipherInitFailed
  | PkeInvalidNonceLength
  | PkeMacMismatch
  deriving (Eq, Show)

generateKeyPair :: IO (PkePublicKey, PkeSecretKey)
generateKeyPair = do
  sk <- Curve25519.generateSecretKey
  let pk = Curve25519.toPublic sk
  pure (PkePublicKey pk, PkeSecretKey sk)

deriveSecretKey :: ByteString -> Either Text PkeSecretKey
deriveSecretKey seed =
  case Curve25519.secretKey seed of
    CryptoFailed err -> Left (T.pack ("failed to derive Curve25519 secret key: " <> show err))
    CryptoPassed sk -> Right (PkeSecretKey sk)

toPublicKey :: PkeSecretKey -> PkePublicKey
toPublicKey (PkeSecretKey sk) = PkePublicKey (Curve25519.toPublic sk)

encrypt ::
  PkePublicKey ->
  ByteString ->
  ByteString ->
  Either PkeError PkeCiphertext
encrypt pk message randomnessSeed =
  fst <$> encryptWithSeeds pk message randomnessSeed

-- | Encrypt and also derive commitment seeds tied to the same shared secret.
encryptWithSeeds ::
  PkePublicKey ->
  ByteString ->
  ByteString ->
  Either PkeError (PkeCiphertext, CommitmentSeeds)
encryptWithSeeds (PkePublicKey serverPk) message randomnessSeed = do
  (ephemeralSk, nonceBytes) <- deriveEphemeral randomnessSeed
  let sharedSecret = Curve25519.dh serverPk ephemeralSk
      sharedBytes = BA.convert sharedSecret :: ByteString
      seeds = deriveCommitmentSeeds sharedBytes
      keyMaterial = BA.convert (hash @_ @SHA512 (sharedBytes <> nonceBytes)) :: ByteString
      (keyBytes, rest) = BS.splitAt 32 keyMaterial
      (nonceAdd, _) = BS.splitAt 12 rest
      nonceCombined =
        if BS.length nonceBytes == 12
          then nonceBytes
          else BS.take 12 (nonceBytes <> nonceAdd)
  chaChaNonce <- case ChaCha.nonce12 nonceCombined of
    CryptoFailed _ -> Left PkeInvalidNonceLength
    CryptoPassed n -> Right n
  state0 <- case ChaCha.initialize keyBytes chaChaNonce of
    CryptoFailed _ -> Left PkeCipherInitFailed
    CryptoPassed st -> Right st
  let (cipherBytes, state1) = ChaCha.encrypt message state0
      auth = ChaCha.finalize state1
      ephemeralPk = Curve25519.toPublic ephemeralSk
      ciphertext =
        PkeCiphertext
          { ephemeralPublic = BA.convert ephemeralPk
          , nonce = nonceCombined
          , payload = cipherBytes
          , authTag = BA.convert auth
          }
  pure (ciphertext, seeds)

decrypt ::
  PkeSecretKey ->
  PkeCiphertext ->
  Either PkeError ByteString
decrypt (PkeSecretKey serverSk) PkeCiphertext {ephemeralPublic, nonce = nonceBytes, payload, authTag} = do
  ephemeralPk <-
    case Curve25519.publicKey ephemeralPublic of
      CryptoFailed _ -> Left PkeInvalidSeed
      CryptoPassed pk -> Right pk
  chaChaNonce <-
    case ChaCha.nonce12 nonceBytes of
      CryptoFailed _ -> Left PkeInvalidNonceLength
      CryptoPassed n -> Right n
  let sharedSecret = Curve25519.dh ephemeralPk serverSk
      sharedBytes = BA.convert sharedSecret :: ByteString
      keyMaterial = BA.convert (hash @_ @SHA512 (sharedBytes <> nonceBytes)) :: ByteString
      (keyBytes, _) = BS.splitAt 32 keyMaterial
  state0 <- case ChaCha.initialize keyBytes chaChaNonce of
    CryptoFailed _ -> Left PkeCipherInitFailed
    CryptoPassed st -> Right st
  let (plainBytes, state1) = ChaCha.decrypt payload state0
      auth = ChaCha.finalize state1
  if BA.convert auth == authTag
    then Right plainBytes
    else Left PkeMacMismatch

deriveEphemeral ::
  ByteString ->
  Either PkeError (Curve25519.SecretKey, ByteString)
deriveEphemeral seed =
  let hashed = BA.convert (hash @_ @SHA512 seed) :: ByteString
      secretSeed = BS.take 32 hashed
      nonceBytes = BS.take 12 (BS.drop 32 hashed <> hashed)
   in case Curve25519.secretKey secretSeed of
        CryptoFailed _ -> Left PkeInvalidSeed
        CryptoPassed sk -> Right (sk, nonceBytes)

-- | Reduce arbitrary bytes to two field elements in the BN254 field used by the Circom circuits.
deriveCommitmentSeeds :: ByteString -> CommitmentSeeds
deriveCommitmentSeeds sharedBytes =
  let digest = BA.convert (hash @_ @SHA512 sharedBytes) :: ByteString
      (sxBytes, syBytes) = BS.splitAt 32 digest
   in CommitmentSeeds
        { seedX = toFieldElement sxBytes
        , seedY = toFieldElement syBytes
        }
  where
    -- BN254 scalar field prime: 21888242871839275222246405745257275088548364400416034343698204186575808495617
    bn254Prime :: Integer
    bn254Prime = 21888242871839275222246405745257275088548364400416034343698204186575808495617

    toFieldElement :: ByteString -> Integer
    toFieldElement bs = bsToInteger bs `mod` bn254Prime

    bsToInteger :: ByteString -> Integer
    bsToInteger = BS.foldl' (\acc b -> acc * 256 + fromIntegral b) 0

serialiseCiphertext :: PkeCiphertext -> ByteString
serialiseCiphertext PkeCiphertext {ephemeralPublic, nonce, payload, authTag} =
  Write.toStrictByteString $
    E.encodeListLen 4
      <> E.encodeBytes ephemeralPublic
      <> E.encodeBytes nonce
      <> E.encodeBytes payload
      <> E.encodeBytes authTag

ciphertextDigest :: PkeCiphertext -> ByteString
ciphertextDigest =
  BA.convert . (hash @_ @SHA512) . serialiseCiphertext

renderError :: PkeError -> Text
renderError = \case
  PkeInvalidSeed -> "invalid Curve25519 seed"
  PkeCipherInitFailed -> "failed to initialise ChaCha20-Poly1305"
  PkeInvalidNonceLength -> "invalid nonce length"
  PkeMacMismatch -> "authentication tag mismatch"

deserialiseCiphertext :: ByteString -> Either Text PkeCiphertext
deserialiseCiphertext bytes = do
  (rest, term) <- first (T.pack . show) (deserialiseFromBytes decodeTerm (BL.fromStrict bytes))
  if BL.null rest
    then fromTerm term
    else Left "unexpected trailing bytes when decoding ciphertext"
  where
    fromTerm :: Term -> Either Text PkeCiphertext
    fromTerm (TList [TBytes eph, TBytes nonceBs, TBytes payloadBs, TBytes authBs]) =
      Right
        PkeCiphertext
          { ephemeralPublic = eph
          , nonce = nonceBs
          , payload = payloadBs
          , authTag = authBs
          }
    fromTerm _ = Left "invalid ciphertext encoding"
