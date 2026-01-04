{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import qualified Cardano.Api as Api
import Data.Aeson (encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Default (def)
import Path (reldir, (</>))
import Path.IO (getCurrentDir, getTempDir, withTempDir)
import WBPS.Core.Cardano.UnsignedTx (UnsignedTx (UnsignedTx))
import WBPS.Core.FileScheme (RootFolders (..), defaultFileScheme)
import WBPS.Core.Keys.ElGamal (AffinePoint (..), EncryptionKey (..), mkRho)
import WBPS.Core.Session.Commitment.Commitment (builCommitment)
import WBPS.Core.Session.Commitment.Scalars (CommitmentScalars (ekPowRho), compute)
import WBPS.Core.ZK.Message (Message (Message), messageToBits)
import WBPS.WBPS (runWBPS)

main :: IO ()
main = do
  let txBodyContent = buildTxBody
  case Api.createTransactionBody Api.ShelleyBasedEraConway txBodyContent of
    Left err -> error (show err)
    Right body -> do
      let unsignedTx = UnsignedTx body
          msg = Message unsignedTx
          mBits = messageToBits def msg
      BL8.putStrLn "unsignedTx"
      BL8.putStrLn (encode unsignedTx)
      BL8.putStrLn "messageBits"
      BL8.putStrLn (encode mBits)
      cwd <- getCurrentDir
      tmp <- getTempDir
      withTempDir tmp "wbps-fixture" $ \outDir -> do
        let scheme = defaultFileScheme RootFolders {input = cwd </> [reldir|wbps|] </> [reldir|setup|], output = outDir}
        let ek =
              EncryptionKey $
                AffinePoint
                  13949409190783008520894738635416501547122416709390247001419320903147870232235
                  6230067313654301039366684823404445124569608018144478198755770506579514903435
        let rho = either (error . show) id (mkRho 1234567890123456789012345678901234567890)
        res <- runWBPS scheme $ do
          cs <- compute ek rho
          builCommitment (ekPowRho cs) msg
        print res

buildTxBody :: Api.TxBodyContent Api.BuildTx Api.ConwayEra
buildTxBody =
  let dummyTxId = case Api.deserialiseFromRawBytes Api.AsTxId (BS.replicate 32 1) of
        Right v -> v
        Left err -> error (show err)
      dummyPkh = case Api.deserialiseFromRawBytes Api.AsPaymentKeyHash (BS.replicate 28 2) of
        Right v -> v
        Left err -> error (show err)
      dummyAddr =
        Api.makeShelleyAddressInEra
          Api.ShelleyBasedEraConway
          Api.Mainnet
          (Api.PaymentCredentialByKey dummyPkh)
          Api.NoStakeAddress
      oneLovelace = Api.quantityToLovelace (Api.Quantity 1)
   in (Api.defaultTxBodyContent Api.ShelleyBasedEraConway)
        { Api.txIns = [(Api.TxIn dummyTxId (Api.TxIx 0), Api.BuildTxWith (Api.KeyWitness Api.KeyWitnessForSpending))]
        , Api.txOuts =
            [ Api.TxOut
                dummyAddr
                (Api.lovelaceToTxOutValue Api.ShelleyBasedEraConway oneLovelace)
                Api.TxOutDatumNone
                Api.ReferenceScriptNone
            ]
        }
