module WBPS.Specs.NominalCase (specs) where

import WBPS.Adapter.CardanoCryptoClass.Crypto (KeyPair (..), Ed25519DSIGN)
import WBPS.Specs.Adapter.GenCardanoKeys (genCardanoKeyPair)
import Path (Abs, Dir, Path, (</>), relfile, reldir)
import Path.IO (ensureDir)
import Test.QuickCheck (Gen, counterexample, forAll, ioProperty, property)
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)
import WBPS ( register, withFileSchemeIO )
 
import Data.Functor ((<&>))
import WBPS.Core.FileScheme
    ( FileScheme, RootFolders, defaultFileScheme )

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
  testProperty "Register - Client can register and obtain verification keys for upcoming proof verification" $
    forAll (genFixtureNominalCase rootFolders) $
      \FixtureNominalCase {signerKeyPair = KeyPair {..}, fileScheme = scheme} ->
        ioProperty $ do
          withFileSchemeIO scheme (register verificationKey)
          <&> \case
              Right _ -> property True
              Left failures ->
                counterexample ("Registration failed: " <> show failures) (property False)
