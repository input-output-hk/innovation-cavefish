module WBPS.Specs.NominalCase (specs) where

import Data.Functor ((<&>))
import Data.List.NonEmpty qualified as NL
import Data.Set qualified as Sets
import Test.QuickCheck (Gen, counterexample, forAll, ioProperty, property, (===))
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)
import WBPS.Core.FileScheme (
  FileScheme,
  RootFolders,
  defaultFileScheme,
 )
import WBPS.Core.Keys.Ed25519 (KeyPair, userWalletPK)
import WBPS.Core.Registration.FetchAccounts (loadAccounts)
import WBPS.Core.Registration.Register (register)
import WBPS.Specs.Adapter.GenCardanoKeys (genEd25519KeyPairs)
import WBPS.WBPS (runWBPS)

data FixtureNominalCase = FixtureNominalCase
  { userWalletKeyPairs :: NL.NonEmpty KeyPair
  , fileScheme :: FileScheme
  }
  deriving (Show)

genFixtureNominalCase :: RootFolders -> Gen FixtureNominalCase
genFixtureNominalCase rootFolders = do
  keyPairs <- genEd25519KeyPairs 4

  pure
    FixtureNominalCase
      { userWalletKeyPairs = keyPairs
      , fileScheme = defaultFileScheme rootFolders
      }

specs :: RootFolders -> TestTree
specs = registerSpecs

registerSpecs :: RootFolders -> TestTree
registerSpecs rootFolders =
  testProperty
    "Register - Client can register and an account is persisted and created for them and all these accounts are retrievable"
    $ forAll (genFixtureNominalCase rootFolders)
    $ \FixtureNominalCase {fileScheme = scheme, ..} ->
      ioProperty $
        do
          runWBPS
            scheme
            ( do
                accountsCreated <- NL.toList <$> mapM (register . userWalletPK) userWalletKeyPairs
                accountsLoaded <- filter (`elem` accountsCreated) <$> loadAccounts
                pure (accountsCreated, accountsLoaded)
            )
          <&> \case
            Right (accountsCreated, accountsLoaded) -> Sets.fromList accountsCreated === Sets.fromList accountsLoaded
            Left failures -> counterexample ("LoadAccounts failed: " <> show failures) (property False)
