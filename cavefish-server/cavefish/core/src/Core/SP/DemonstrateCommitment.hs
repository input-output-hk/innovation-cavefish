module Core.SP.DemonstrateCommitment (handle, Inputs (..), Outputs (..), Commitment (..)) where

import Cardano.Api qualified as Api
import Control.Concurrent.STM (atomically, modifyTVar')
import Control.Monad (unless)
import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader (ask), runReaderT)
import Core.Api.AppContext (
  AppM,
  Env (..),
 )
import Core.Api.State (
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
import Core.Cbor (serialiseTx)
import Core.Intent (
  BuildTxResult (BuildTxResult, mockState, tx, txAbs),
  IntentDSL,
  satisfies,
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
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time.Clock (addUTCTime, getCurrentTime)
import GHC.Generics (Generic)
import Servant (
  err400,
  err422,
  err500,
  errBody,
  throwError,
 )
import WBPS.Core.BuildCommitment (
  BuildCommitmentInput (BuildCommitmentInput),
  BuildCommitmentOutput (maskedChunks),
  ComId (unComId),
  computeComId,
  runBuildCommitment,
 )
import WBPS.Core.BuildCommitment qualified as BuildCommitment
import WBPS.Core.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Core.Keys.ElGamal qualified as ElGamal
import WBPS.Core.Primitives.Circom (
  BuildCommitmentParams (BuildCommitmentParams),
  defCommitmentParams,
 )
import WBPS.Registration (AccountCreated (..))
import WBPS.Registration qualified as WBPS

data Inputs = Inputs
  { userWalletPublicKey :: UserWalletPublicKey
  , intent :: IntentDSL
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype CommitmentId = CommitmentId ByteString
  deriving (Eq, Show, Generic)

data Commitment
  = Commitment {id :: ByteString, payload :: [Integer]}
  deriving (Eq, Show, Generic)

instance ToJSON Commitment where
  toJSON Commitment {..} =
    object
      [ "id" .= renderHex id
      , "payload" .= payload
      ]

instance FromJSON Commitment where
  parseJSON = withObject "Commitment" $ \o -> do
    idHex :: Text <- o .: "id"
    payload <- o .: "payload"
    id <- parseHex idHex
    pure Commitment {..}

data Outputs = Outputs
  { commitment :: Commitment
  , txAbs :: TxAbs Api.ConwayEra
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

handle :: Inputs -> AppM Outputs
handle Inputs {userWalletPublicKey, intent} = do
  Env {ttl, build, wbpsScheme} <- ask
  BuildTxResult {tx = tx, txAbs = txAbs, mockState = builtState} <- build intent

  liftIO (WBPS.withFileSchemeIO wbpsScheme (WBPS.loadAccount userWalletPublicKey))
    >>= \case
      (Left e) -> throwError err500 {errBody = BL8.pack ("Unexpected event" ++ show e)}
      (Right Nothing) -> throwError err400 {errBody = BL8.pack "Account Not registered"}
      (Right (Just AccountCreated {encryptionKeys = ElGamal.KeyPair {ek}})) -> do

-- auxNonceBytes :: ByteString <- liftIO $ getRandomBytes 32
-- rhoBytes :: ByteString <- liftIO $ getRandomBytes 32

-- let message = Message . serialiseTx $ tx
--     toServerErr msg = err500 {errBody = BL.fromStrict (TE.encodeUtf8 msg)}
--     toPkeErr err = err500 {errBody = BL.fromStrict (TE.encodeUtf8 ("pke encryption failed: " <> renderError err))}
-- -- The part from the paper: C ← PKE.Enc(ek, m; ρ) with ek = pkePublic, m = serialiseTx tx <> auxNonceBytes
-- (ciphertext, CommitmentSeeds {seedX, seedY}) <-
--   liftEither $ first toPkeErr (encryptWithSeeds ek message rhoBytes)
-- -- Run the BuildCommitment circuit to derive comTx. comTx is the list of out_masked_chunk values
-- -- (each packed message limb plus its Poseidon-derived mask).
-- -- Parameters cover max payload (16KB tx + 32B nonce): 131,328 bits.
-- -- We use 252-bit limbs to avoid mod-p aliasing and stay aligned with the wbps_cardano verifier circuit.
-- let commitmentParams@(BuildCommitmentParams msgSize _ _) = defCommitmentParams
--     messageBits =
--       take msgSize (payloadBits message ++ repeat 0)
-- let bcInput =
--       BuildCommitmentInput seedX seedY messageBits
-- comTx <-
--   liftIO $
--     runReaderT
--       (runBuildCommitment commitmentParams bcInput)
--       wbpsScheme
--       >>= either (ioError . userError) (pure . maskedChunks)
-- let comIdVal = computeComId (BuildCommitment.Commitment comTx)

-- let txBody = Api.getTxBody tx
--     txId = Api.getTxId txBody

-- pure
--   Outputs
--     { txAbs
--     , commitment = Commitment (unComId comIdVal) comTx
--     }

newtype Message = Message ByteString

buildCommitment :: UserWalletPublicKey -> Message -> Commitment
buildCommitment userWalletPublicKey (Message message) = do

-- -- Convert a bytestring to a little-endian bit vector.
-- payloadBits :: ByteString -> [Int]
-- payloadBits bs = concatMap byteToBits (BS.unpack bs)
--   where
--     byteToBits b =
--       [ if testBit b i then 1 else 0
--       | i <- [0 .. 7]
--       ]
