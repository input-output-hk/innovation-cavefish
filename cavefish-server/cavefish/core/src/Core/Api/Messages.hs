{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Core.Api.Messages where

import Cardano.Api qualified as Api
import Control.Concurrent.STM (TVar, atomically, modifyTVar', readTVar, readTVarIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader (ask))
import Core.Api.AppContext (
  AppM,
  Env (..),
 )
import Core.Api.State (
  Completed (Completed, creator, submittedAt, tx),
  Pending (
    Pending,
    auxNonce,
    ciphertext,
    creator,
    expiry,
    tx,
    txAbsHash
  ),
 )
import Core.Pke (
  PkeSecretKey,
  decrypt,
  renderError,
 )
import Core.Proof (renderHex)
import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toJSON),
  Value (String),
  object,
  withObject,
  (.:),
  (.=),
 )
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Ledger.Tx.CardanoAPI (CardanoTx, pattern CardanoEmulatorEraTx)
import Servant (
  ServerError,
  err400,
  err500,
  errBody,
  throwError,
 )
import WBPS.Core.Cardano.Cbor
import WBPS.Core.Keys.Ed25519 qualified as Ed25519

-- | Cavefish API a
data TransactionResp
  = TransactionMissing
  | TransactionPending PendingSummary
  | TransactionSubmitted SubmittedSummary
  deriving (Eq, Show, Generic)

data PendingSummary = PendingSummary
  { pendingExpiresAt :: UTCTime
  , pendingClientId :: Ed25519.UserWalletPublicKey
  }
  deriving (Eq, Show, Generic)

data SubmittedSummary = SubmittedSummary
  { submittedTx :: CardanoTx
  , submittedAt :: UTCTime
  , submittedClientId :: Ed25519.UserWalletPublicKey
  }
  deriving (Eq, Show, Generic)

instance ToJSON TransactionResp where
  toJSON = \case
    TransactionMissing ->
      object ["status" .= String "missing"]
    TransactionPending PendingSummary {..} ->
      object
        [ "status" .= String "pending"
        , "expiresAt" .= pendingExpiresAt
        , "clientId" .= pendingClientId
        ]
    TransactionSubmitted SubmittedSummary {..} ->
      object
        [ "status" .= String "submitted"
        , "transaction" .= submittedTx
        , "submittedAt" .= submittedAt
        , "clientId" .= submittedClientId
        ]

instance FromJSON TransactionResp where
  parseJSON = withObject "TransactionResp" $ \o -> do
    status :: Text <- o .: "status"
    case status of
      "missing" -> pure TransactionMissing
      "pending" -> do
        expiresAt <- o .: "expiresAt"
        clientId <- o .: "clientId"
        pure $ TransactionPending (PendingSummary expiresAt clientId)
      "submitted" -> do
        tx <- o .: "transaction"
        submittedAt <- o .: "submittedAt"
        clientId <- o .: "clientId"
        pure $ TransactionSubmitted (SubmittedSummary tx submittedAt clientId)
      _ -> fail "unknown transaction status"

newtype PendingResp = PendingResp
  { pending :: [PendingItem]
  }
  deriving (Eq, Show, Generic)

instance ToJSON PendingResp

instance FromJSON PendingResp

data PendingItem = PendingItem
  { txId :: Text
  , txAbsHash :: Text
  , expiresAt :: UTCTime
  , clientId :: Ed25519.UserWalletPublicKey
  }
  deriving (Eq, Show, Generic)

instance ToJSON PendingItem

instance FromJSON PendingItem

pendingH :: AppM PendingResp
pendingH = do
  Env {pending} <- ask
  pendings <- liftIO . atomically $ Map.toAscList <$> readTVar pending
  pure . PendingResp $ fmap (uncurry mkPendingItem) pendings
  where
    mkPendingItem :: Api.TxId -> Pending -> PendingItem
    mkPendingItem txId Pending {creator, txAbsHash, expiry} =
      PendingItem
        { txId = Api.serialiseToRawBytesHexText txId
        , txAbsHash = renderHex txAbsHash
        , expiresAt = expiry
        , clientId = creator
        }

transactionH :: Text -> AppM TransactionResp
transactionH txIdText = do
  Env {complete, pending} <- ask
  case parseTxIdHex txIdText of
    Nothing -> throwError err400 {errBody = "malformed tx id"}
    Just txId -> do
      completes <- Map.lookup txId <$> liftIO (readTVarIO complete)
      pendings <- lookupPendingEntry pending txId
      let toSubmitted Completed {tx, submittedAt, creator = creatorId} =
            TransactionSubmitted (SubmittedSummary (CardanoEmulatorEraTx tx) submittedAt creatorId)
          toPending Pending {expiry, creator = creatorId} =
            TransactionPending (PendingSummary expiry creatorId)
      let fallback = maybe TransactionMissing toPending pendings
      pure $ maybe fallback toSubmitted completes

finaliseSigTag :: ByteString
finaliseSigTag = "cavefish/finalise/v1"

clientSignatureMessage :: ByteString -> ByteString
clientSignatureMessage txAbsHash = finaliseSigTag <> txAbsHash

verifyClientSignature :: Ed25519.UserWalletPublicKey -> ByteString -> ByteString -> Bool
verifyClientSignature _ _ _ = False -- to be implemented

decryptPendingPayload :: PkeSecretKey -> Pending -> Either ServerError ByteString
decryptPendingPayload pkeSecret Pending {ciphertext = pendingCiphertext, auxNonce = pendingAuxNonce, tx} =
  case decrypt pkeSecret pendingCiphertext of
    Left err ->
      let msg = "pke decrypt failed: " <> renderError err
       in Left err500 {errBody = BL.fromStrict (TE.encodeUtf8 msg)}
    Right payload ->
      let txBytes = serialiseTx tx
          (payloadTxBytes, payloadNonce) = BS.splitAt (BS.length txBytes) payload
       in if payloadTxBytes /= txBytes
            then
              Left
                err500
                  { errBody =
                      "ciphertext payload mismatch: transaction bytes differ"
                  }
            else
              if payloadNonce /= pendingAuxNonce
                then
                  Left
                    err500
                      { errBody =
                          "ciphertext payload mismatch: aux nonce differ"
                      }
                else Right payload

parseTxIdHex :: Text -> Maybe Api.TxId
parseTxIdHex =
  either (const Nothing) Just
    . Api.deserialiseFromRawBytesHex @Api.TxId
    . TE.encodeUtf8

lookupPendingEntry :: TVar (Map Api.TxId Pending) -> Api.TxId -> AppM (Maybe Pending)
lookupPendingEntry store txId =
  liftIO . atomically $ do
    entries <- readTVar store
    pure (Map.lookup txId entries)

removePendingEntry :: TVar (Map Api.TxId Pending) -> Api.TxId -> AppM ()
removePendingEntry store txId =
  liftIO . atomically $ modifyTVar' store (Map.delete txId)
