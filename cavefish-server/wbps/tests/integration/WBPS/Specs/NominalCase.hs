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
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (KeyPair, userWalletPK)
import WBPS.Core.Registration.FetchAccounts (loadAllRegistered)
import WBPS.Core.Registration.Register (register)
import WBPS.Core.Registration.RegistrationId (RegistrationId (RegistrationId))
import WBPS.Core.Session.FetchSession (loadSessions)
import WBPS.Core.Session.Session (Session (Demonstrated))
import WBPS.Core.Session.Steps.Demonstration.Demonstrate (demonstrate)
import WBPS.Core.Setup.Circuit.FileScheme (
  defaultFileScheme,
 )
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
        , "    - 2. Demonstrate a Commitment - Where Prepared Message,Scalars and Commitment will be generated and saved to disk"
        ]
    )
    $ forAll genFixtureNominalCase
    $ \FixtureNominalCase {userWalletKeyPairs} ->
      ioProperty $ do
        (rootFolders, fileScheme) <- (id &&& defaultFileScheme) <$> getRootFolder [reldir|integration-wpbs-nominal-flow|]
        runWBPS
          fileScheme
          ( do
              accountsCreated <- NL.toList <$> mapM (register . userWalletPK) userWalletKeyPairs
              accountsLoaded <- filter (`elem` accountsCreated) <$> loadAllRegistered
              anUnsignedTx <- liftIO (readFixture . unsignedTxFixture . commitmentFixtures $ rootFolders)
              demonstrationHistories <- NL.toList <$> mapM (flip demonstrate anUnsignedTx . RegistrationId . userWalletPK) userWalletKeyPairs
              let demonstratedSessions = Demonstrated . snd <$> demonstrationHistories
              sessionsLoaded <- filter (`elem` demonstratedSessions) <$> loadSessions
              pure (accountsCreated, accountsLoaded, demonstratedSessions, sessionsLoaded)
          )
          <&> \case
            Right (accountsCreated, accountsLoaded, demonstratedSessions, sessionsLoaded) ->
              counterexample "Loaded accounts mismatch" (Sets.fromList accountsCreated === Sets.fromList accountsLoaded)
                .&&. counterexample "Loaded sessions mismatch" (Sets.fromList sessionsLoaded === Sets.fromList demonstratedSessions)
            Left failures -> counterexample ("LoadAllRegistered failed: " <> show failures) (property False)
