module WBPS.Specs.Core.Keys.Ed25519 (specs) where

import Data.Aeson (eitherDecode, encode)
import Data.ByteString.Lazy.Char8 qualified as BL8
import Test.QuickCheck (Property, counterexample, ioProperty, (===))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 qualified as Ed25519

specs :: TestTree
specs =
  testGroup
    "WBPS.Core.Registration.Artefacts.Keys.Ed25519"
    [ testProperty "PublicKey JSON roundtrips" publicKeyJsonRoundtrips
    ]

publicKeyJsonRoundtrips :: Property
publicKeyJsonRoundtrips = ioProperty $ do
  keyPair <- Ed25519.generateKeyPair
  let pk = Ed25519.getPublicKey keyPair
      encoded = encode pk
  pure $
    case eitherDecode encoded of
      Right (decoded :: Ed25519.PublicKey) -> decoded === pk
      Left err ->
        counterexample
          ("Failed to decode PublicKey JSON: " <> err <> " from payload " <> BL8.unpack encoded)
          False
