{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module ClientBackend.Handlers (
  verifyH,
  satisfiesH,
  registerHelperH,
  prepareHelperH,
  commitHelperH,
  finaliseHelperH,
  payToIntentH,
  demoAddressesH,
) where

import Cardano.Api qualified as Api
import Client.Mock (decodeHex, mkFinaliseReq, mkPrepareReq)
import ClientBackend.App (AppM)
import ClientBackend.Helpers
import ClientBackend.Types
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Core.Api.Messages
import Core.Cbor (ClientWitnessBundle (..), deserialiseClientWitnessBundle)
import Core.Intent (ChangeDelta, IntentW (..), satisfies, toInternalIntent)
import Core.PaymentProof (ProofResult, hashTxAbs, verifyPaymentProof)
import Core.Proof (renderHex)
import Core.TxAbs (TxAbs)
import Crypto.PubKey.Ed25519 qualified as Ed
import Data.Bifunctor (first)
import Data.ByteArray qualified as BA
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TE

verifyCommitProof :: Ed.PublicKey -> PrepareResp -> ProofResult -> Either Text ()
verifyCommitProof publicKey PrepareResp {txId = txIdText, txAbs, witnessBundleHex} pi = do
  witnessBytes <- decodeHex "witness bundle" witnessBundleHex
  ClientWitnessBundle {cwbCiphertext = ciphertext, cwbAuxNonce = auxNonceBytes, cwbTxId = bundleTxId} <-
    first (const "failed to decode witness bundle") (deserialiseClientWitnessBundle witnessBytes)
  txId <-
    case Api.deserialiseFromRawBytesHex @Api.TxId (TE.encodeUtf8 txIdText) of
      Left err -> Left (Text.pack ("failed to deserialise tx id: " <> show err))
      Right v -> Right v
  let expectedTxIdBytes = Api.serialiseToRawBytes txId
  when (bundleTxId /= expectedTxIdBytes) $ Left "witness bundle tx id mismatch"
  verifyPaymentProof publicKey pi txAbs txId ciphertext auxNonceBytes

verifyH :: VerifyReq -> AppM VerifyResp
verifyH VerifyReq {publicKey = pk, prepared, proofResult} =
  pure . VerifyResp $
    verifyCommitProof pk prepared proofResult

verifySatisfies :: IntentW -> TxAbs Api.ConwayEra -> ChangeDelta -> Either Text Bool
verifySatisfies intentW txAbs changeDelta = do
  internal <- toInternalIntent intentW
  pure (satisfies changeDelta internal txAbs)

satisfiesH :: SatisfiesReq -> AppM SatisfiesResp
satisfiesH SatisfiesReq {intent, txAbs, changeDelta} =
  pure $
    SatisfiesResp $
      verifySatisfies intent txAbs changeDelta

registerHelperH :: RegisterHelperReq -> AppM RegisterHelperResp
registerHelperH RegisterHelperReq {secretKey} = do
  secretResult <- resolveSecret secretKey
  pure . RegisterHelperResp $
    do
      (secret, secretHex) <- secretResult
      let registerReq = RegisterReq {publicKey = Ed.toPublic secret}
      pure
        RegisterHelperPayload
          { registerReq = registerReq
          , clientSecret = secretHex
          }

prepareHelperH :: PrepareHelperReq -> AppM PrepareHelperResp
prepareHelperH PrepareHelperReq {clientId, intent} =
  pure . PrepareHelperResp $
    mkPrepareReq clientId intent

commitHelperH :: CommitHelperReq -> AppM CommitHelperResp
commitHelperH CommitHelperReq {txId} = do
  secretResult <- resolveSecret Nothing
  pure . CommitHelperResp $
    do
      (secret, secretHex) <- secretResult
      let bigR = Ed.toPublic secret
          commitReq = CommitReq {txId = txId, bigR}
      pure
        CommitHelperPayload
          { commitReq = commitReq
          , littleR = secretHex
          }

finaliseHelperH :: FinaliseHelperReq -> AppM FinaliseHelperResp
finaliseHelperH FinaliseHelperReq {secretKey, helperPrepareResp = PrepareResp {txId, txAbs}} =
  pure . FinaliseHelperResp $
    do
      sk <- parseSecretKey secretKey
      let txAbsHash = hashTxAbs txAbs
      pure (mkFinaliseReq sk txId txAbsHash)

payToIntentH :: PayToIntentReq -> AppM IntentHelperResp
payToIntentH
  PayToIntentReq
    { payToOutputs
    , spendFromAddress
    , changeToAddress
    , payToMaxFee
    , payToMaxInterval
    } =
    pure . IntentHelperResp $ do
      payToClauses <- traverse mkPayTo payToOutputs
      spendW <- maybe (Right demoFundingAddressW) parseAddress spendFromAddress
      changeW <- maybe (Right demoChangeAddressW) parseAddress changeToAddress
      feeW <- traverse validateFee payToMaxFee
      intervalW <- traverse validateInterval payToMaxInterval
      let firstPayTo :| restPayTos = payToClauses
          extras =
            restPayTos
              ++ [SpendFromW spendW, ChangeToW changeW]
              ++ maybe [] (\fee -> [MaxFeeW fee]) feeW
              ++ maybe [] (\interval -> [MaxIntervalW interval]) intervalW
      pure $ AndExpsW (firstPayTo :| extras)
    where
      mkPayTo :: PayToOutputReq -> Either Text IntentW
      mkPayTo PayToOutputReq {amount, address} = do
        when (amount <= 0) $ Left "amount must be positive"
        addrW <- parseAddress address
        let value = Api.lovelaceToValue (fromInteger amount)
        pure (PayToW value addrW)
      validateFee n
        | n <= 0 = Left "max fee must be positive"
        | otherwise = Right n
      validateInterval n
        | n <= 0 = Left "max interval must be positive"
        | otherwise = Right n

demoAddressesH :: AppM DemoAddressesResp
demoAddressesH = pure (DemoAddressesResp demoAddresses)

resolveSecret :: Maybe Text -> AppM (Either Text (Ed.SecretKey, Text))
resolveSecret maybeSecret =
  case maybeSecret of
    Nothing -> liftIO generateSecret
    Just secretHex ->
      pure $
        do
          sk <- parseSecretKey secretHex
          pure (sk, renderHex (BA.convert sk))
