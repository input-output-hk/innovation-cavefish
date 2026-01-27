{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import qualified Cardano.Api as Api
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Default (def)
import Path (reldir, relfile, (</>))
import qualified Path
import qualified Path.IO as P
import WBPS.Adapter.Math.AffinePoint (AffinePoint (..))
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (generateKeyPair, userWalletPK)
import WBPS.Core.Registration.Artefacts.Keys.ElGamal (EncryptionKey (..))
import WBPS.Core.Registration.RegistrationId (RegistrationId (RegistrationId))
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Cardano.UnsignedTx (UnsignedTx (UnsignedTx))
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Commitment (Commitment (..), CommitmentPayload (..))
import qualified WBPS.Core.Session.Steps.Demonstration.Artefacts.Commitment.Build as CommitmentBuild
import WBPS.Core.Session.Steps.Demonstration.Artefacts.PreparedMessage (Message (Message))
import WBPS.Core.Session.Steps.Demonstration.Artefacts.PreparedMessage.Prepare (toBitsPaddedToMaxSize)
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Rho (mkRho)
import qualified WBPS.Core.Session.Steps.Demonstration.Artefacts.Scalars as Scalars
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Scalars.Compute (compute)
import WBPS.Core.Setup.Circuit.FileScheme (RootFolders (..), defaultFileScheme)
import WBPS.WBPS (runWBPS)

main :: IO ()
main = do
  txBody <- createBody
  let unsignedTx = UnsignedTx txBody
      message = Message unsignedTx
      messageBits = toBitsPaddedToMaxSize def message

  fixturesDir <- P.makeAbsolute =<< pure ([reldir|wbps/tests/integration/fixtures/commitment|])
  P.ensureDir fixturesDir
  BL8.writeFile (Path.toFilePath (fixturesDir </> [relfile|unsignedTx.json|])) (encode unsignedTx)
  BL8.writeFile (Path.toFilePath (fixturesDir </> [relfile|messageBits.json|])) (encode messageBits)

  -- compute commitment for reference
  let ek =
        EncryptionKey $
          AffinePoint
            13949409190783008520894738635416501547122416709390247001419320903147870232235
            6230067313654301039366684823404445124569608018144478198755770506579514903435
      rho = either (error . show) Prelude.id (mkRho 1234567890123456789012345678901234567890)

  registrationId <- RegistrationId . userWalletPK <$> generateKeyPair
  tmpRoot <- P.getTempDir
  P.withTempDir tmpRoot "wbps-fixture" $ \outDir -> do
    cwd <- P.getCurrentDir
    let scheme = defaultFileScheme RootFolders {input = cwd </> [reldir|wbps|] </> [reldir|setup|], output = outDir}
    res <- runWBPS scheme $ do
      Scalars.Scalars {Scalars.ekPowRho = ekPowRho'} <- compute ek rho
      CommitmentBuild.build registrationId CommitmentBuild.Input {CommitmentBuild.ekPowRho = ekPowRho', CommitmentBuild.messageBits = messageBits}
    case res of
      Left e -> error (show e)
      Right Commitment {payload = CommitmentPayload payload'} ->
        BL8.writeFile (Path.toFilePath (fixturesDir </> [relfile|commitment.json|])) (encode payload')

createBody :: IO (Api.TxBody Api.ConwayEra)
createBody = do
  dummyTxId <- case Api.deserialiseFromRawBytes Api.AsTxId (BS.replicate 32 1) of
    Right v -> pure v
    Left err -> fail (show err)
  dummySk <- Api.generateSigningKey Api.AsPaymentKey
  let dummyVk = Api.getVerificationKey dummySk
      dummyAddr =
        Api.makeShelleyAddressInEra
          Api.ShelleyBasedEraConway
          Api.Mainnet
          (Api.PaymentCredentialByKey (Api.verificationKeyHash dummyVk))
          Api.NoStakeAddress
      oneLovelace = Api.quantityToLovelace (Api.Quantity 1)
      txBodyContent =
        (Api.defaultTxBodyContent Api.ShelleyBasedEraConway)
          { Api.txIns = [(Api.TxIn dummyTxId (Api.TxIx 0), Api.BuildTxWith (Api.KeyWitness Api.KeyWitnessForSpending))]
          , Api.txOuts =
              [ Api.TxOut
                  dummyAddr
                  (Api.lovelaceToTxOutValue Api.ShelleyBasedEraConway oneLovelace)
                  Api.TxOutDatumNone
                  Api.ReferenceScriptNone
              ]
          }
  case Api.createTransactionBody Api.ShelleyBasedEraConway txBodyContent of
    Left err -> fail (show err)
    Right body -> pure body
