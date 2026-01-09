{-# LANGUAGE QuasiQuotes #-}

module WBPS.Specs.NominalCase (specs) where

import Control.Arrow ((&&&))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Functor ((<&>))
import Data.List.NonEmpty qualified as NL
import Data.Set qualified as Sets
import Path (reldir)
import Test.QuickCheck (Gen, counterexample, forAll, ioProperty, property, (.&&.), (===))
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck (testProperty)
import WBPS.Core.FileScheme (
  defaultFileScheme,
 )
import WBPS.Core.Keys.Ed25519 (KeyPair, userWalletPK)
import WBPS.Core.Registration.FetchAccounts (loadAccounts)
import WBPS.Core.Registration.Register (register)
import WBPS.Core.Session.Demonstrate (demonstrate)
import WBPS.Core.Session.FetchSession (loadSessions)
import WBPS.Specs.Adapter.Fixture (
  CommitmentFixtures (unsignedTxFixture),
  commitmentFixtures,
  readFixture,
 )
import WBPS.Specs.Adapter.GenCardanoKeys (genEd25519KeyPairs)
import WBPS.Specs.Adapter.Test (getRootFolder)
import WBPS.WBPS (runWBPS)

newtype FixtureNominalCase = FixtureNominalCase
  { userWalletKeyPairs :: NL.NonEmpty KeyPair
  }
  deriving (Show)

genFixtureNominalCase :: Gen FixtureNominalCase
genFixtureNominalCase = do
  keyPairs <- genEd25519KeyPairs 4
  pure
    FixtureNominalCase
      { userWalletKeyPairs = keyPairs
      }

specs :: TestTree
specs =
  testProperty
    ( unlines
        [ "- 1. Register - Where a client can register and an account is persisted and created for them and all these accounts are retrievable"
        , "    - 2. Create Session - Where Message/Rho/Scalars and Commitment will be generated and saved"
        ]
    )
    $ forAll genFixtureNominalCase
    $ \FixtureNominalCase {userWalletKeyPairs} ->
      ioProperty $ do
        (rootFolders, fileScheme) <- (id &&& defaultFileScheme) <$> getRootFolder [reldir|WBPS-integration-specs-Register|]
        runWBPS
          fileScheme
          ( do
              accountsCreated <- NL.toList <$> mapM (register . userWalletPK) userWalletKeyPairs
              accountsLoaded <- filter (`elem` accountsCreated) <$> loadAccounts
              anUnsignedTx <- liftIO (readFixture . unsignedTxFixture . commitmentFixtures $ rootFolders)
              sessionsCreated <- NL.toList <$> mapM (flip demonstrate anUnsignedTx . userWalletPK) userWalletKeyPairs
              sessionsLoaded <- filter (`elem` sessionsCreated) <$> loadSessions
              pure (accountsCreated, accountsLoaded, sessionsCreated, sessionsLoaded)
          )
          <&> \case
            Right (accountsCreated, accountsLoaded, sessionsCreated, sessionsLoaded) ->
              counterexample "Loaded accounts mismatch" (Sets.fromList accountsCreated === Sets.fromList accountsLoaded)
                .&&. counterexample "Loaded sessions mismatch" (Sets.fromList sessionsLoaded === Sets.fromList sessionsCreated)
            Left failures -> counterexample ("LoadAccounts failed: " <> show failures) (property False)
