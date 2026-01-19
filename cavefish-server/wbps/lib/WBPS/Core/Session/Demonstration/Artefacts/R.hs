module WBPS.Core.Session.Demonstration.Artefacts.R (
  R (..),
  RSecret (..),
  deriveR,
  generateKeyTuple,
  toWord8s,
) where

import Cardano.Crypto.DSIGN (deriveVerKeyDSIGN)
import Control.Monad.IO.Class (MonadIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.String (IsString)
import Data.Word (Word8)
import WBPS.Adapter.CardanoCryptoClass.Crypto qualified as Adapter
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 qualified as Ed25519

newtype R = R Ed25519.PublicKey
  deriving newtype (Eq, Show, Ord, FromJSON, ToJSON, IsString)

-- | Secret nonce corresponding to a public R value.
newtype RSecret = RSecret Ed25519.PrivateKey
  deriving newtype (Eq, Show, FromJSON, ToJSON, IsString)

-- | Derive the public nonce R from its secret counterpart.
deriveR :: RSecret -> R
deriveR (RSecret privateKey) =
  R (derivePublicKey privateKey)

generateKeyTuple :: MonadIO m => m (RSecret, R)
generateKeyTuple = do
  (privateKey, publicKey) <- Ed25519.generateKeyTuple
  pure (RSecret privateKey, R publicKey)

toWord8s :: R -> [Word8]
toWord8s (R rPk) =
  Ed25519.userWalletPublicKeyToWord8s (Ed25519.UserWalletPublicKey rPk)

derivePublicKey :: Ed25519.PrivateKey -> Ed25519.PublicKey
derivePublicKey (Ed25519.PrivateKey (Adapter.PrivateKey sk)) =
  Ed25519.PublicKey (Adapter.PublicKey (deriveVerKeyDSIGN sk))
