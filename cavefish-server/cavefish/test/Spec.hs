{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Spec (spec) where

import qualified Cardano.Api as Api
import Cardano.Api.Shelley (PlutusScript (..))
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Control.Monad.Identity (Identity (..))
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State.Strict (runStateT)
import Control.Monad.Trans.Writer (WriterT (..))
import Cooked (MockChain, MockChainError (..), MockChainT (..), Wallet, wallet)
import Cooked.MockChain (MonadBlockChainWithoutValidation (..), registerStakingCred)
import Cooked.MockChain.MockChainState (MockChainState (..), mockChainState0)
import Cooked.Skeleton (TxSkelOut, txSkelOutValue)
import Core.Intent (AddressW (..), BuildTxResult (..), ChangeDelta, Env (..), Intent (..), IntentW (..), ScriptSpec (..), satisfies, toInternalIntent)
import Core.Proof (mkProof)
import Core.TxAbs (TxAbs (..), cardanoTxToTxAbs)
import Crypto.Error (CryptoFailable (..))
import qualified Crypto.PubKey.Ed25519 as Ed
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Short (fromShort)
import Data.Default (def)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Ledger.Scripts (StakeValidator (getStakeValidator))
import Ledger.Tx (
  pattern CardanoEmulatorEraTx,
 )
import Ledger.Tx.CardanoAPI (toCardanoAddressInEra, toCardanoValue)
import Observers.Observer (intentStakeValidatorBytes, stakeValidatorFromBytes)
import qualified Plutus.Script.Utils.Address as ScriptAddr
import Plutus.Script.Utils.Scripts (Language (PlutusV2), Versioned (..))
import Servant
import Sp.Server (FinaliseReq (..), FinaliseResp (..), FinaliseResult (..), PrepareReq (..), PrepareResp (..), finaliseH, hashTxAbs, prepareH)
import Sp.State (Pending (..), PendingStore)
import Sp.TxBuilder (buildTx)
import Test.Hspec

runHandlerOrFail :: Handler a -> IO a
runHandlerOrFail handler = do
  result <- runExceptT (runHandler' handler)
  either (fail . show) pure result

runMockChainPure :: MockChainState -> MockChain a -> (Either MockChainError a, MockChainState)
runMockChainPure st action =
  let ((result, newState), _) =
        runIdentity $
          runWriterT $
            runStateT (runExceptT (unMockChain action)) st
   in (result, newState)

initialMockState :: MockChainState
initialMockState =
  case runMockChainPure def mockChainState0 of
    (Left err, _) -> error ("failed to initialise mock chain state: " <> show err)
    (Right st, _) -> st

mkEnvWithBuild ::
  PendingStore ->
  TVar MockChainState ->
  (Env -> Intent -> ByteString -> IO BuildTxResult) ->
  (Env -> Api.Tx Api.ConwayEra -> MockChainState -> IO (Either Text ())) ->
  Env
mkEnvWithBuild pendingStore _mockState buildFn submitFn = env
 where
  env =
    Env
      { spSk = testSecretKey
      , pending = pendingStore
      , ttl = 3600
      , spWallet = testSpWallet
      , resolveWallet = \case
          ((==) testPayToAddress -> True) -> Just testPayToWallet
          ((==) testSpendFromAddress -> True) -> Just testSpendFromWallet
          _ -> Nothing
      , spFee = 0
      , build = buildFn env
      , submit = submitFn env
      , scriptReg = mockRegistry
      }

buildTxBuild ::
  TVar MockChainState ->
  Env ->
  Intent ->
  ByteString ->
  IO BuildTxResult
buildTxBuild mockState env intent observerBytes = do
  st0 <- readTVarIO mockState
  let (result, st1) =
        runMockChainPure st0 $ do
          let stakeValidator = stakeValidatorFromBytes observerBytes
              cred =
                ScriptAddr.toCredential
                  (Versioned (getStakeValidator stakeValidator) PlutusV2)
          registerStakingCred cred 0 0
          buildTx intent observerBytes env
  case result of
    Left err -> fail ("buildTx failed: " <> show err)
    Right cardanoTx@(CardanoEmulatorEraTx tx) -> do
      let txAbs = cardanoTxToTxAbs cardanoTx
          consumed = consumedTotal st0 st1
          produced = producedTotal (outputs txAbs)
          delta = consumed <> Api.negateValue produced
      pure
        BuildTxResult
          { tx = tx
          , changeDelta = delta
          , txAbs = txAbs
          , mockState = st1
          }

consumedTotal ::
  MockChainState -> MockChainState -> ChangeDelta
consumedTotal stateBefore stateAfter =
  mconcat
    [ toConwayValue out
    | (oref, (out, True)) <- Map.toList (mcstOutputs stateBefore)
    , let mAfter = Map.lookup oref (mcstOutputs stateAfter)
    , consumedInAfter mAfter
    ]
 where
  toConwayValue txOut =
    case toCardanoValue (txSkelOutValue txOut) of
      Left err -> error ("failed to convert consumed output value: " <> show err)
      Right valueInEra -> valueInEra

consumedInAfter :: Maybe (TxSkelOut, Bool) -> Bool
consumedInAfter Nothing = True
consumedInAfter (Just (_, present)) = not present

producedTotal ::
  [Api.TxOut Api.CtxTx Api.ConwayEra] ->
  ChangeDelta
producedTotal outs =
  mconcat
    [ Api.txOutValueToValue val
    | Api.TxOut _ val _ _ <- outs
    ]

submitWithCooked :: TVar MockChainState -> Env -> Api.Tx Api.ConwayEra -> MockChainState -> IO (Either Text ())
submitWithCooked mockState _ _ newState = do
  atomically $ writeTVar mockState newState
  pure (Right ())

spec :: Spec
spec = do
  describe "buildTx integration" $ do
    it "prepare -> finalise roundtrip uses buildTx" $ do
      pendingStore <- newTVarIO Map.empty
      mockStateVar <- newTVarIO initialMockState
      internalIntent <-
        case toInternalIntent (intent buildTxPrepareReq) of
          Left err -> expectationFailure (show err) >> fail "invalid buildTx intent"
          Right intent -> pure intent

      -- TODO WG: We can't do this exactly, but it'd be nice to check coherance of the observer with the intent
      -- expectedObserverBytes <-
      --   case intentStakeValidatorBytes internalIntent of
      --     Left err -> expectationFailure (show err) >> fail "invalid observer"
      --     Right bytes -> pure bytes
      -- expectedObserverBytes `shouldBe` buildTxPrepareReq.observer
      let env =
            mkEnvWithBuild
              pendingStore
              mockStateVar
              (buildTxBuild mockStateVar)
              (submitWithCooked mockStateVar)
      BuildTxResult
        { tx = expectedTx
        , txAbs = expectedTxAbs
        , changeDelta = expectedDelta
        } <-
        buildTxBuild mockStateVar env internalIntent buildTxPrepareReq.observer
      let expectedTxIdValue = Api.getTxId (Api.getTxBody expectedTx)
          expectedTxId = Api.serialiseToRawBytesHexText expectedTxIdValue
          expectedTxAbsHash = hashTxAbs expectedTxAbs
          expectedProof = mkProof (spSk env) expectedTxIdValue expectedTxAbsHash
      PrepareResp{txId = gotTxId, txAbs = gotTxAbs, proof = gotProof} <-
        runHandlerOrFail (prepareH env buildTxPrepareReq)
      gotTxId `shouldBe` expectedTxId
      gotTxAbs `shouldBe` expectedTxAbs
      gotProof `shouldBe` expectedProof
      satisfies env expectedDelta internalIntent gotTxAbs `shouldBe` True
      pendingMap <- readTVarIO pendingStore
      case Map.lookup expectedTxIdValue pendingMap of
        Nothing -> expectationFailure "pending entry not stored"
        Just Pending{tx, txAbsHash} -> do
          tx `shouldBe` expectedTx
          txAbsHash `shouldBe` expectedTxAbsHash
          let storedTxAbs = cardanoTxToTxAbs (CardanoEmulatorEraTx tx)
          satisfies env expectedDelta internalIntent storedTxAbs `shouldBe` True
          let storedProof = mkProof (spSk env) expectedTxIdValue txAbsHash
          gotProof `shouldBe` storedProof
      let finaliseReq = finaliseReqDummySig gotTxId
      FinaliseResp{txId = finalTxId, result = finalResult} <-
        runHandlerOrFail (finaliseH env finaliseReq)
      pendingAfter <- readTVarIO pendingStore
      Map.notMember expectedTxIdValue pendingAfter `shouldBe` True
      finalTxId `shouldBe` gotTxId
      finalResult `shouldBe` Finalised

-- Dummy registry for WIP
mockRegistry :: [ScriptSpec]
mockRegistry = [payV1]

payV1 :: ScriptSpec
payV1 =
  ScriptSpec
    { ssId = testSpendFromAddress
    , ssEval = const True -- TODO WG: Enhance realism
    }

buildTxPrepareReq :: PrepareReq
buildTxPrepareReq =
  prepareReqFromIntentW
    ( AndExpsW
        ( PayToW testPayToValue (AddressW testPayToAddressText)
            :| [SpendFromW (AddressW testSpendFromAddressText)]
        )
    )

prepareReqFromIntentW :: IntentW -> PrepareReq
prepareReqFromIntentW iw =
  let intent = internalIntentOrError iw
      observerBytes =
        case intentStakeValidatorBytes intent of
          Left err -> error ("prepareReqFromIntentW: " <> Text.unpack err)
          Right bs -> bs
   in PrepareReq{intent = iw, observer = observerBytes}

internalIntentOrError :: IntentW -> Intent
internalIntentOrError iw =
  case toInternalIntent iw of
    Left err -> error ("internalIntentOrError: " <> Text.unpack err)
    Right intent -> intent

finaliseReqDummySig :: Text -> FinaliseReq
finaliseReqDummySig txId = FinaliseReq{txId = txId, lcSig = BS.replicate 64 0}

testNetworkId :: Api.NetworkId
testNetworkId = Api.Testnet (Api.NetworkMagic 1)

testPayToWallet :: Wallet
testPayToWallet = wallet 3

testPayToAddress :: Api.AddressInEra Api.ConwayEra
testPayToAddress =
  case toCardanoAddressInEra testNetworkId (ScriptAddr.toAddress testPayToWallet) of
    Left err -> error ("failed to derive pay-to address: " <> show err)
    Right addr -> addr

testPayToAddressText :: Text
testPayToAddressText = Api.serialiseAddress testPayToAddress

testPayToValue :: Api.Value
testPayToValue = Api.lovelaceToValue 2

testSpendFromWallet :: Wallet
testSpendFromWallet = wallet 2

testSpendFromAddress :: Api.AddressInEra Api.ConwayEra
testSpendFromAddress =
  case toCardanoAddressInEra testNetworkId (ScriptAddr.toAddress testSpendFromWallet) of
    Left err -> error ("failed to derive pay-to address: " <> show err)
    Right addr -> addr

testSpendFromAddressText :: Text
testSpendFromAddressText = Api.serialiseAddress testSpendFromAddress

testSpWallet :: Wallet
testSpWallet = wallet 1

testObserverBytes :: ByteString
testObserverBytes =
  let PlutusScriptSerialised scriptBytes =
        Api.examplePlutusScriptAlwaysSucceeds Api.WitCtxStake
   in fromShort scriptBytes

testSecretKey :: Ed.SecretKey
testSecretKey =
  case Ed.secretKey (BS.pack [1 .. 32]) of
    CryptoPassed sk -> sk
    CryptoFailed err -> error ("invalid static secret key: " <> show err)
