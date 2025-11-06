module WBPS.Specs.NominalCase (specs) where

import Data.Functor ((<&>))
import Path (Abs, Dir, Path, reldir, relfile, (</>))
import Path.IO (ensureDir)
import Test.QuickCheck (Gen, counterexample, forAll, ioProperty, property)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)
import WBPS (register, withFileSchemeIO)
import WBPS.Adapter.CardanoCryptoClass.Crypto (Ed25519DSIGN, KeyPair (..))
import WBPS.Core.FileScheme (
  FileScheme,
  RootFolders,
  defaultFileScheme,
 )
import WBPS.Specs.Adapter.GenCardanoKeys (genCardanoKeyPair)

data FixtureNominalCase = FixtureNominalCase
  { signerKeyPair :: KeyPair Ed25519DSIGN
  , fileScheme :: FileScheme
  }
  deriving (Show)

genFixtureNominalCase :: RootFolders -> Gen FixtureNominalCase
genFixtureNominalCase rootFolders = do
  keyPair <- genCardanoKeyPair
  pure
    FixtureNominalCase
      { signerKeyPair = keyPair
      , fileScheme = defaultFileScheme rootFolders
      }

specs :: RootFolders -> TestTree
specs rootFolders =
  testProperty
    "Register - Client can register and obtain verification keys for upcoming proof verification" $
    forAll (genFixtureNominalCase rootFolders) $
      \FixtureNominalCase {signerKeyPair = KeyPair {..}, fileScheme = scheme} ->
        ioProperty $
          do
            withFileSchemeIO scheme (register verificationKey)
            <&> \case
              Right _ -> property True
              Left failures ->
                counterexample ("Registration failed: " <> show failures) (property False)
