module WBPS.Core.BuildChallenge where

import Crypto.Hash (Digest, SHA512, hash)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS

buildChallenge ::
  ByteString ->
  ByteString ->
  ByteString ->
  Either String (Digest SHA512)
buildChallenge rBytes xBytes txIdBytes
  | BS.length rBytes /= 32 = Left "expected 32-byte commitment point"
  | BS.length xBytes /= 32 = Left "expected 32-byte signer key"
  | otherwise =
      Right (hash (BS.concat [rBytes, xBytes, txIdBytes]))
