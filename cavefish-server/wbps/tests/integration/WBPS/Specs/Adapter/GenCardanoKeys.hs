{-# LANGUAGE TypeApplications #-}

module WBPS.Specs.Adapter.GenCardanoKeys (
  genEd25519KeyPair,
  genEd25519KeyPairs,
) where

import Cardano.Crypto.DSIGN (seedSizeDSIGN)
import Cardano.Crypto.Seed (Seed, mkSeedFromBytes)
import Data.ByteString qualified as BS
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NL
import Data.Proxy (Proxy (..))
import Test.QuickCheck (Gen, arbitrary, vectorOf)
import Test.QuickCheck.Gen (listOf1)
import WBPS.Adapter.CardanoCryptoClass.Crypto as Adapter
import WBPS.Core.Keys.Ed25519 as Ed25519

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

genEd25519KeyPair :: Gen Ed25519.KeyPair
genEd25519KeyPair = do
  (x, y) <- genEd25519Keypair
  return
    ( Ed25519.KeyPair
        (Adapter.KeyPair {signatureKey = PrivateKey x, verificationKey = Adapter.PublicKey y})
    )

genEd25519KeyPairs :: Int -> Gen (NonEmpty Ed25519.KeyPair)
genEd25519KeyPairs n = NL.fromList . take n <$> listOf1 genEd25519KeyPair
