{-# LANGUAGE OverloadedStrings #-}

module WBPS.Specs.Session.Demonstration.PreparedMessage.Prepare (specs) where

import Control.Exception (IOException, try)
import Control.Monad.Except (runExceptT)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Default (def)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)
import Test.Tasty.QuickCheck (Gen, Property, counterexample, forAll, ioProperty, testProperty, (===))
import WBPS.Core.Failure (WBPSFailure (TxBuiltTooLarge, TxInputsCountMismatch))
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Cardano.UnsignedTx (
  PrivateTxInputs (PrivateTxInputs),
  UnsignedTx,
  extractPrivateElements,
  randomizeTxAndPadItToCircuitMessageSize,
 )
import WBPS.Core.Session.Steps.Demonstration.Artefacts.PreparedMessage (
  PreparedMessage (PreparedMessage, circuit),
 )
import WBPS.Core.Session.Steps.Demonstration.Artefacts.PreparedMessage.Prepare (
  prepare,
  recompose,
 )
import WBPS.Core.Setup.Circuit.Parameters (
  CircuitMessageMaxSize (CircuitMessageMaxSize),
  CircuitParameters (messageSize, txInputSize),
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
    (mkParams fixtureMismatchedInputCount)
    isTxInputsCountMismatch
    "TxInputsCountMismatch"

prepareWithMessageTooLarge :: Assertion
prepareWithMessageTooLarge =
  assertPrepareTooLarge (mkParams fixtureInputCount)

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

mkParams :: Int -> CircuitParameters
mkParams inputCount =
  (def :: CircuitParameters)
    { txInputSize =
        either
          (error . ("mkParams: " <>))
          id
          (mkCircuitTxInputSize inputCount)
    }

genCircuitParameters :: Gen CircuitParameters
genCircuitParameters =
  pure (mkParams fixtureInputCount)

assertPrepareTooLarge :: CircuitParameters -> Assertion
assertPrepareTooLarge params = do
  unsignedTx <- loadFixtureUnsignedTx
  oversizedTx <- buildOversizedTx params unsignedTx
  result <- try (runExceptT (prepare params oversizedTx)) :: IO (Either IOException (Either [WBPSFailure] PreparedMessage))
  case result of
    Left _ -> pure ()
    Right (Left [failure]) | isTxBuiltTooLarge failure -> pure ()
    Right failures -> assertFailure ("Expected prepare to fail with TxBuiltTooLarge, got: " <> show failures)

buildOversizedTx :: CircuitParameters -> UnsignedTx -> IO UnsignedTx
buildOversizedTx params unsignedTx =
  let CircuitMessageMaxSize baseBits = messageSize params
      messageSizeMultipleBits = 1016
      candidates = [baseBits + messageSizeMultipleBits * n | n <- [1 .. 3]]
   in go candidates
  where
    go [] =
      ioError . userError $
        "Unable to build an oversized tx for prepareWithMessageTooLarge."
    go (targetBits : rest) = do
      result <- try (randomizeTxAndPadItToCircuitMessageSize targetBits unsignedTx) :: IO (Either IOException UnsignedTx)
      case result of
        Left _ -> go rest
        Right paddedTx -> pure paddedTx

fixtureUnsignedTxJson :: BL8.ByteString
fixtureUnsignedTxJson =
  "{\"cborHex\":\"84a300d9010281825820000000000000000000000000000000000000000000000000000000000000000000018182581d618b218424ad74df25d35c2ea8e094a4c5c5aeb2cbb4424193315693131a000f42400200a0f5f6\",\"description\":\"\",\"type\":\"TxBodyConway\"}"

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
