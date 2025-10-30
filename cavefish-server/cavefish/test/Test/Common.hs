{-# LANGUAGE OverloadedStrings #-}

module Test.Common where

import Cardano.Api qualified as Api
import Cooked (Wallet, wallet)
import Core.Intent (AddressW (..), IntentW (..))
import Core.Pke (PkeSecretKey, deriveSecretKey)
import Crypto.Error (CryptoFailable (..))
import Crypto.PubKey.Ed25519 qualified as Ed
import Data.ByteString qualified as BS
import Data.List.NonEmpty
import Data.Text (Text)
import Ledger.Tx.CardanoAPI (toCardanoAddressInEra)
import Plutus.Script.Utils.Address qualified as ScriptAddr

testIntentW :: IntentW
testIntentW =
  AndExpsW
    ( PayToW testPayToValue (AddressW testPayToAddressText)
        :| [SpendFromW (AddressW testSpendFromAddressText)]
    )

testNetworkId :: Api.NetworkId
testNetworkId = Api.Testnet (Api.NetworkMagic 1)

testPayToWallet :: Wallet
testPayToWallet = wallet 3

testPayToAddress :: Api.AddressInEra Api.ConwayEra
testPayToAddress =
  case toCardanoAddressInEra testNetworkId (ScriptAddr.toAddress testPayToWallet) of
    Left err -> error ("failed to derive pay-to address: " <> show err)
    Right addr -> addr

testPayToAddressText :: Text
testPayToAddressText = Api.serialiseAddress testPayToAddress

testPayToValue :: Api.Value
testPayToValue = Api.lovelaceToValue 2

testSpendFromWallet :: Wallet
testSpendFromWallet = wallet 2

testSpendFromAddress :: Api.AddressInEra Api.ConwayEra
testSpendFromAddress =
  case toCardanoAddressInEra testNetworkId (ScriptAddr.toAddress testSpendFromWallet) of
    Left err -> error ("failed to derive spend-from address: " <> show err)
    Right addr -> addr

testSpendFromAddressText :: Text
testSpendFromAddressText = Api.serialiseAddress testSpendFromAddress

testSpWallet :: Wallet
testSpWallet = wallet 1

testClientSecretKey :: Ed.SecretKey
testClientSecretKey =
  case Ed.secretKey (BS.pack [101 .. 132]) of
    CryptoPassed sk -> sk
    CryptoFailed err -> error ("invalid client secret key: " <> show err)

testSecretKey :: Ed.SecretKey
testSecretKey =
  case Ed.secretKey (BS.pack [1 .. 32]) of
    CryptoPassed sk -> sk
    CryptoFailed err -> error ("invalid static secret key: " <> show err)

testCommitSecretKey :: Ed.SecretKey
testCommitSecretKey =
  case Ed.secretKey (BS.pack [133 .. 164]) of
    CryptoPassed sk -> sk
    CryptoFailed err -> error ("invalid commit secret key: " <> show err)

testPkeSecretKey :: PkeSecretKey
testPkeSecretKey =
  case deriveSecretKey (BS.pack [33 .. 64]) of
    Left err -> error ("invalid PKE secret key: " <> show err)
    Right sk -> sk
