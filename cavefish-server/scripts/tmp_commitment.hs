{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import qualified Cardano.Api as Api
import qualified Cardano.Api.Shelley as Api
import qualified Data.ByteString as BS
import Path (reldir, (</>))
import Path.IO (getCurrentDir, getTempDir, withTempDir)
import System.Random (mkStdGen)
import WBPS.Core.Cardano.UnsignedTx (UnsignedTx (..))
import WBPS.Core.FileScheme (RootFolders (..), defaultFileScheme)
import WBPS.Core.Keys.ElGamal (AffinePoint (..), EncryptionKey (..), mkRho)
import WBPS.Core.Session.Commitment.Build (build)
import WBPS.Core.Session.Commitment.Commitment (Message (..))
import WBPS.Core.Session.Commitment.Scalars (CommitmentScalars (..), compute)
import WBPS.WBPS (runWBPS)

main :: IO ()
main = do
  cwd <- getCurrentDir
  tempRoot <- getTempDir
  withTempDir tempRoot "wbps-test" $ \outDir -> do
    let scheme = defaultFileScheme RootFolders {input = cwd </> [reldir|wbps|] </> [reldir|inputs|], output = outDir}
    let ek =
          EncryptionKey $
            AffinePoint
              13949409190783008520894738635416501547122416709390247001419320903147870232235
              6230067313654301039366684823404445124569608018144478198755770506579514903435
    let rho = either error id (mkRho 1234567890123456789012345678901234567890)
    case Api.makeTransactionBody txBodyContent of
      Left err -> putStrLn $ "Tx body error: " <> show err
      Right body -> do
        let unsignedTx = UnsignedTx body
        let msg = Message unsignedTx
        res <- runWBPS scheme $ do
          cs <- compute ek rho
          build (ekPowRho cs) msg
        print res
  where
    dummyTxId = case Api.deserialiseFromRawBytes Api.AsTxId (BS.replicate 32 1) of
      Left e -> error (show e)
      Right v -> v
    (dummySk, _) = Api.generateInsecureSigningKey (mkStdGen 42) Api.AsPaymentKey
    dummyVk = Api.getVerificationKey dummySk
    dummyAddr = Api.makeShelleyAddressInEra Api.ShelleyBasedEraConway Api.Mainnet (Api.PaymentCredentialByKey (Api.verificationKeyHash dummyVk)) Api.NoStakeAddress
    txBodyContent :: Api.TxBodyContent Api.BuildTx Api.ConwayEra
    txBodyContent =
      (Api.defaultTxBodyContent Api.ShelleyBasedEraConway)
        { Api.txIns = [(Api.TxIn dummyTxId (Api.TxIx 0), Api.BuildTxWith (Api.KeyWitness Api.KeyWitnessForSpending))]
        , Api.txOuts = [Api.TxOut dummyAddr (Api.lovelaceToTxOutValue Api.ShelleyBasedEraConway (Api.Lovelace 1)) Api.TxOutDatumNone Api.ReferenceScriptNone]
        }
