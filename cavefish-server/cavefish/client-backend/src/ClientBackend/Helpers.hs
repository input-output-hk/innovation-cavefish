{-# LANGUAGE TypeApplications #-}

module ClientBackend.Helpers (
  demoAddresses,
  demoChangeAddressW,
  demoFundingAddressW,
  parseAddress,
  parseSecretKey,
  generateSecret,
) where

import Cardano.Api qualified as Api
import Client.Mock (decodeHex)
import Cooked (Wallet, wallet)
import Core.Intent (AddressW (..))
import Core.Proof (renderHex)
import Crypto.Error (CryptoFailable (..))
import Crypto.PubKey.Ed25519 qualified as Ed
import Crypto.Random (getRandomBytes)
import Data.ByteArray qualified as BA
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Ledger.Tx.CardanoAPI (toCardanoAddressInEra)
import Plutus.Script.Utils.Address qualified as ScriptAddr

demoNetworkId :: Api.NetworkId
demoNetworkId = Api.Testnet (Api.NetworkMagic 1)

demoWallets :: [Wallet]
demoWallets =
  [ wallet 1
  , wallet 2
  , wallet 3
  , wallet 4
  , wallet 5
  ]

demoAddresses :: [Text]
demoAddresses = mapMaybe walletAddressText demoWallets

demoFundingAddressW :: AddressW
demoFundingAddressW =
  AddressW $
    fromMaybe (error "failed to derive demo funding address") $
      walletAddressText (wallet 1)

demoChangeAddressW :: AddressW
demoChangeAddressW =
  AddressW $
    fromMaybe (error "failed to derive demo change address") $
      walletAddressText (wallet 2)

walletAddressText :: Wallet -> Maybe Text
walletAddressText w =
  case toCardanoAddressInEra demoNetworkId (ScriptAddr.toAddress w) of
    Left _ -> Nothing
    Right addr -> Just (Api.serialiseAddress addr)

parseAddress :: Text -> Either Text AddressW
parseAddress txt =
  case Api.deserialiseAddress (Api.AsAddressInEra Api.AsConwayEra) txt of
    Nothing -> Left "invalid address"
    Just _ -> Right (AddressW txt)

parseSecretKey :: Text -> Either Text Ed.SecretKey
parseSecretKey secretHex = do
  bytes <- decodeHex "secret key" secretHex
  case Ed.secretKey bytes of
    CryptoFailed err -> Left (Text.pack ("invalid secret key: " <> show err))
    CryptoPassed sk -> Right sk

generateSecret :: IO (Either Text (Ed.SecretKey, Text))
generateSecret = do
  bytes <- getRandomBytes @IO @BA.ScrubbedBytes Ed.secretKeySize
  pure $
    case Ed.secretKey bytes of
      CryptoFailed err -> Left (Text.pack ("failed to derive secret key: " <> show err))
      CryptoPassed sk ->
        let secretHex = renderHex (BA.convert sk)
         in Right (sk, secretHex)
