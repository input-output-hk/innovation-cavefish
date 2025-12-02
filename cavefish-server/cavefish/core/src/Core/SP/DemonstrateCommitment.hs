{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Core.SP.DemonstrateCommitment (handle, Inputs (..), Outputs (..)) where

import Cardano.Api qualified as Api
import Control.Concurrent.STM (atomically, modifyTVar', readTVar)
import Control.Monad (unless)
import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader (ask), runReaderT)
import Core.Api.AppContext (
  AppM,
  Env (
    Env,
    build,
    clientRegistration,
    pending,
    pkePublic,
    ttl,
    wbpsScheme
  ),
 )
import Core.Api.State (
  ClientId,
  Pending (
    Pending,
    auxNonce,
    challenge,
    ciphertext,
    comId,
    comTx,
    commitment,
    creator,
    expiry,
    message,
    mockState,
    rho,
    tx,
    txAbsHash
  ),
 )
import Core.Cbor (mkWitnessBundle, serialiseClientWitnessBundle, serialiseTx)
import Core.Intent (
  BuildTxResult (BuildTxResult, changeDelta, mockState, tx, txAbs),
  ChangeDelta,
  IntentW,
  satisfies,
  toInternalIntent,
 )
import Core.PaymentProof (hashTxAbs)
import Core.Pke (
  CommitmentSeeds (CommitmentSeeds, seedX, seedY),
  encryptWithSeeds,
  renderError,
 )
import Core.Proof (parseHex, renderHex)
import Core.TxAbs (TxAbs)
import Crypto.Random (getRandomBytes)
import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toJSON),
  object,
  withObject,
  (.:),
  (.=),
 )
import Data.Bifunctor (first)
import Data.Bits (testBit)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (addUTCTime, getCurrentTime)
import GHC.Base (when)
import GHC.Generics (Generic)
import Servant (
  err403,
  err422,
  err500,
  errBody,
  throwError,
 )
import WBPS.Core.BuildCommitment (
  BuildCommitmentInput (BuildCommitmentInput),
  BuildCommitmentOutput (maskedChunks),
  ComId (unComId),
  Commitment (Commitment),
  computeComId,
  runBuildCommitment,
 )
import WBPS.Core.Primitives.Circom (BuildCommitmentParams (BuildCommitmentParams))

data Inputs = Inputs
  { intent :: IntentW
  , observer :: Maybe ByteString
  , clientId :: ClientId
  }
  deriving (Eq, Show, Generic)

instance FromJSON Inputs where
  parseJSON = withObject "Inputs" $ \o -> do
    intent <- o .: "intent"
    obsHex :: Maybe Text <- o .: "observer"
    observer <- traverse parseHex obsHex
    clientId <- o .: "clientId"
    pure Inputs {..}

instance ToJSON Inputs where
  toJSON Inputs {..} =
    object
      [ "intent" .= intent
      , "observer" .= fmap renderHex observer
      , "clientId" .= clientId
      ]

data Outputs = Outputs
  { txId :: Text
  , txAbs :: TxAbs Api.ConwayEra
  , -- We need to include a `consumed - produced` here, otherwise the client can't run `satisfies` for `ChangeTo`
    changeDelta :: ChangeDelta
  , comId :: ByteString
  , comTx :: [Integer]
  , witnessBundleHex :: Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Outputs where
  parseJSON = withObject "Outputs" $ \o -> do
    txId <- o .: "txId"
    txAbs <- o .: "txAbs"
    changeDelta <- o .: "changeDelta"
    comIdHex :: Text <- o .: "comId"
    comId <- parseHex comIdHex
    comTx <- o .: "comTx"
    witnessBundleHex <- o .: "witnessBundleHex"
    pure Outputs {..}

instance ToJSON Outputs where
  toJSON Outputs {..} =
    object
      [ "txId" .= txId
      , "txAbs" .= txAbs
      , "changeDelta" .= changeDelta
      , "comId" .= renderHex comId
      , "comTx" .= comTx
      , "witnessBundleHex" .= witnessBundleHex
      ]

handle :: Inputs -> AppM Outputs
handle Inputs {..} = do
  Env {pending, clientRegistration, ttl, pkePublic, build, wbpsScheme} <- ask
  internalIntent <- liftIO $ either (ioError . userError . T.unpack) pure (toInternalIntent intent)

  clientKnown <- liftIO . atomically $ do
    registry <- readTVar clientRegistration
    pure (Map.member clientId registry)
  unless clientKnown $
    throwError err403 {errBody = "unknown client"}

  -- TODO WG: We can't do this exactly, but it'd be nice to say at this point whether or not the observer is coherent with the intent
  -- expectedObserverBytes <-
  --   liftIO $ either (ioError . userError . T.unpack) pure (intentStakeValidatorBytes intent)

  -- when (observer /= expectedObserverBytes) $
  --   throwError err422{errBody = "observer script does not match intent"}

  BuildTxResult {tx = tx, txAbs = txAbs, mockState = builtState, changeDelta = cd} <-
    liftIO $ build internalIntent observer

  auxNonceBytes :: ByteString <- liftIO $ getRandomBytes 32
  rhoBytes :: ByteString <- liftIO $ getRandomBytes 32

  let payload = serialiseTx tx <> auxNonceBytes
      toServerErr msg = err500 {errBody = BL.fromStrict (TE.encodeUtf8 msg)}
      toPkeErr err = err500 {errBody = BL.fromStrict (TE.encodeUtf8 ("pke encryption failed: " <> renderError err))}
  -- The part from the paper: C ← PKE.Enc(ek, m; ρ) with ek = pkePublic, m = serialiseTx tx <> auxNonceBytes
  (ciphertext, CommitmentSeeds {seedX, seedY}) <-
    liftEither $ first toPkeErr (encryptWithSeeds pkePublic payload rhoBytes)
  -- Run the BuildCommitment circuit to derive comTx. comTx is the list of out_masked_chunk values
  -- (each packed message limb plus its Poseidon-derived mask).
  -- Parameters cover max payload (16KB tx + 32B nonce): 131,328 bits.
  -- We use 252-bit limbs to avoid mod-p aliasing and stay aligned with the wbps_cardano verifier circuit.
  let commitmentParams@(BuildCommitmentParams msgSize _ _) = BuildCommitmentParams 131544 252 522
      messageBits =
        take msgSize (payloadBits payload ++ repeat 0)
  let bcInput =
        BuildCommitmentInput seedX seedY messageBits
  comTx <-
    liftIO $
      runReaderT
        (runBuildCommitment commitmentParams bcInput)
        wbpsScheme
        >>= either (ioError . userError) (pure . maskedChunks)
  let comIdVal = computeComId (Commitment comTx)
  witnessBundle <-
    liftEither $ first toServerErr $ mkWitnessBundle tx txAbs observer auxNonceBytes ciphertext
  let witnessBundleHex = renderHex (serialiseClientWitnessBundle witnessBundle)

  unless (satisfies cd internalIntent txAbs) $
    throwError err422 {errBody = "transaction does not satisfy intent"}

  let txBody = Api.getTxBody tx
      txId = Api.getTxId txBody
      txIdTxt = Api.serialiseToRawBytesHexText txId
      txAbsHash :: ByteString
      txAbsHash = hashTxAbs txAbs
  now <- liftIO getCurrentTime
  let expiry = addUTCTime ttl now
      pendingEntry =
        Pending
          { tx
          , txAbsHash
          , expiry
          , mockState = builtState
          , creator = clientId
          , ciphertext
          , auxNonce = auxNonceBytes
          , rho = rhoBytes
          , message = payload
          , comId = comIdVal
          , comTx
          , commitment = Nothing
          , challenge = Nothing
          }
  liftIO . atomically $
    modifyTVar' pending $
      Map.insert txId pendingEntry

  pure
    Outputs
      { txId = txIdTxt
      , txAbs
      , changeDelta = cd
      , comId = unComId comIdVal
      , comTx
      , witnessBundleHex
      }

-- Convert a bytestring to a little-endian bit vector.
payloadBits :: ByteString -> [Int]
payloadBits bs =
  concatMap byteToBits (BS.unpack bs)
  where
    byteToBits b =
      [ if testBit b i then 1 else 0
      | i <- [0 .. 7]
      ]
