{-# LANGUAGE TypeApplications #-}

module WBPS.Specs.Adapter.GenCardanoKeys (
  genEd25519Seed,
  genEd25519Keypair,
  genCardanoKeyPair,
) where

import Cardano.Crypto.DSIGN (seedSizeDSIGN)
import Cardano.Crypto.Seed (Seed, mkSeedFromBytes)
import Data.ByteString qualified as BS
import Data.Proxy (Proxy (..))
import Test.QuickCheck (Gen, arbitrary, vectorOf)
import WBPS.Adapter.CardanoCryptoClass.Crypto

-- | Generate a DSIGN seed of the correct size for Ed25519 (pure).
genEd25519Seed :: Gen Seed
genEd25519Seed = do
  let n = fromIntegral (seedSizeDSIGN (Proxy @Ed25519DSIGN))
  bytes <- BS.pack <$> vectorOf n arbitrary
  pure (mkSeedFromBytes bytes)

-- | Generate an Ed25519 keypair (pure). Perfect for property tests.
genEd25519Keypair :: Gen (SignKeyDSIGN Ed25519DSIGN, VerKeyDSIGN Ed25519DSIGN)
genEd25519Keypair = do
  seed <- genEd25519Seed
  let sk = genKeyDSIGN @Ed25519DSIGN seed
      vk = deriveVerKeyDSIGN sk
  pure (sk, vk)

genCardanoKeyPair :: Gen (KeyPair Ed25519DSIGN)
genCardanoKeyPair = do
  (x, y) <- genEd25519Keypair
  return KeyPair {signatureKey = PrivateKey x, verificationKey = PublicKey y}
