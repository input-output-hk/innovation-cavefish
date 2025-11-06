module WBPS.Core (SignerKey) where

import Cardano.Crypto.DSIGN.Ed25519 (Ed25519DSIGN)
import WBPS.Adapter.CardanoCryptoClass.Crypto (PublicKey)

type SignerKey = PublicKey Ed25519DSIGN
