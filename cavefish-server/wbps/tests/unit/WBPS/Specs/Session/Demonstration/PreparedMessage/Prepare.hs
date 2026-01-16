{-# LANGUAGE OverloadedStrings #-}

module WBPS.Specs.Session.Demonstration.PreparedMessage.Prepare (specs) where

import Control.Monad.Except (runExceptT)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy.Char8 qualified as BL8
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)
import Test.Tasty.QuickCheck (Gen, Property, counterexample, elements, forAll, ioProperty, testProperty, (===))
import WBPS.Core.Failure (WBPSFailure (TxBuiltTooLarge, TxInputsCountMismatch))
import WBPS.Core.Session.Demonstration.Artefacts.Cardano.UnsignedTx (PrivateTxInputs (PrivateTxInputs), UnsignedTx, extractPrivateElements)
import WBPS.Core.Session.Demonstration.Artefacts.PreparedMessage (
  PreparedMessage (PreparedMessage, circuit),
 )
import WBPS.Core.Session.Demonstration.Artefacts.PreparedMessage.Prepare (
  prepare,
  recompose,
 )
import WBPS.Core.Setup.Circuit.Parameters (
  CircuitMessageMaxSize (CircuitMessageMaxSize),
  CircuitParameters (CircuitParameters, messageSize, txInputSize),
  mkCircuitTxInputSize,
 )

specs :: TestTree
specs =
  testGroup
    "PreparedMessage.prepare"
    [ testProperty "prepare/recompose roundtrip returns the same PreparedMessage" infoPreservedWhenPreparedAndRecomposed
    , testCase "fails when circuit mistmatch with tx inputs" prepareWithInputMismatch
    , testCase "fails when tx exceeds circuit message size" prepareWithMessageTooLarge
    ]

infoPreservedWhenPreparedAndRecomposed :: Property
infoPreservedWhenPreparedAndRecomposed =
  forAll genCircuitParameters $ \params -> ioProperty $ do
    unsignedTx <- loadFixtureUnsignedTx
    runExceptT
      ( do
          preparedMessage@PreparedMessage {circuit = circuitMessage} <- prepare params unsignedTx
          recomposedMessage <- recompose params circuitMessage
          pure (preparedMessage, recomposedMessage)
      )
      >>= \case
        Left failures ->
          pure (counterexample ("Expected prepare/recompose to succeed, got: " <> show failures) False)
        Right (preparedMessage, recomposedMessage) ->
          pure (preparedMessage === recomposedMessage)

prepareWithInputMismatch :: Assertion
prepareWithInputMismatch =
  assertPrepareFailure
    (mkParams 800 fixtureMismatchedInputCount)
    isTxInputsCountMismatch
    "TxInputsCountMismatch"

prepareWithMessageTooLarge :: Assertion
prepareWithMessageTooLarge =
  assertPrepareFailure
    (mkParams 50 fixtureInputCount)
    isTxBuiltTooLarge
    "TxBuiltTooLarge"

assertPrepareFailure ::
  CircuitParameters ->
  (WBPSFailure -> Bool) ->
  String ->
  Assertion
assertPrepareFailure params matchesFailure label = do
  unsignedTx <- loadFixtureUnsignedTx
  runExceptT (prepare params unsignedTx) >>= \case
    Left [failure] | matchesFailure failure -> pure ()
    Left failures -> assertFailure ("Expected " <> label <> ", got: " <> show failures)
    Right _ -> assertFailure ("Expected prepare to fail with " <> label <> ", but it succeeded")

isTxInputsCountMismatch :: WBPSFailure -> Bool
isTxInputsCountMismatch = \case
  TxInputsCountMismatch _ -> True
  _ -> False

isTxBuiltTooLarge :: WBPSFailure -> Bool
isTxBuiltTooLarge = \case
  TxBuiltTooLarge _ -> True
  _ -> False

mkParams :: Int -> Int -> CircuitParameters
mkParams messageSizeBytes inputCount =
  CircuitParameters
    { messageSize = CircuitMessageMaxSize (messageSizeBytes * 8)
    , txInputSize =
        either
          (error . ("mkParams: " <>))
          id
          (mkCircuitTxInputSize inputCount)
    }

genCircuitParameters :: Gen CircuitParameters
genCircuitParameters =
  mkParams <$> elements [400, 800, 1200] <*> pure fixtureInputCount

fixtureUnsignedTxJson :: BL8.ByteString
fixtureUnsignedTxJson =
  "{\"cborHex\":\"84a300d9010281825820010101010101010101010101010101010101010101010101010101010101010100018182581d611cca48da7f291b81a09ddc0e132fce0c5a1ef3410e5a70f9fcd5f777010200a0f5f6\",\"description\":\"\",\"type\":\"TxBodyConway\"}"

fixtureUnsignedTx :: UnsignedTx
fixtureUnsignedTx =
  case eitherDecode fixtureUnsignedTxJson of
    Left err -> error ("Failed to decode UnsignedTx fixture: " <> err)
    Right tx -> tx

fixtureInputCount :: Int
fixtureInputCount =
  let (PrivateTxInputs inputs, _) = extractPrivateElements fixtureUnsignedTx
   in length inputs

fixtureMismatchedInputCount :: Int
fixtureMismatchedInputCount =
  if fixtureInputCount == 1 then 2 else 1

loadFixtureUnsignedTx :: IO UnsignedTx
loadFixtureUnsignedTx = pure fixtureUnsignedTx
