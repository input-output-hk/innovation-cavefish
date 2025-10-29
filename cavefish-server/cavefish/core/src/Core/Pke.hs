{-# LANGUAGE OverloadedStrings #-}

module Core.Pke (
  PkeSecretKey (..),
  PkePublicKey (..),
  PkeCiphertext (..),
  PkeError (..),
  generateKeyPair,
  deriveSecretKey,
  toPublicKey,
  encrypt,
  decrypt,
  serialiseCiphertext,
  ciphertextDigest,
  renderError,
  deserialiseCiphertext,
) where

import Codec.CBOR.Encoding qualified as E
import Codec.CBOR.Read (deserialiseFromBytes)
import Codec.CBOR.Term (Term (..), decodeTerm)
import Codec.CBOR.Write qualified as Write
import Crypto.Cipher.ChaChaPoly1305 qualified as ChaCha
import Crypto.Error (CryptoFailable (..))
import Crypto.Hash (SHA512 (..), hash)
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
encrypt (PkePublicKey serverPk) message randomnessSeed = do
  (ephemeralSk, nonceBytes) <- deriveEphemeral randomnessSeed
  let sharedSecret = Curve25519.dh serverPk ephemeralSk
      sharedBytes = BA.convert sharedSecret :: ByteString
      keyMaterial = BA.convert (hash @_ @SHA512 (sharedBytes <> nonceCombined)) :: ByteString
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
  pure
    PkeCiphertext
      { ephemeralPublic = BA.convert ephemeralPk
      , nonce = nonceCombined
      , payload = cipherBytes
      , authTag = BA.convert auth
      }

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
