module Adapter.Cavefish.Client (
  setupCavefish,
  Setup (..),
  ServiceProviderAPI (..),
  UserToolkitAPI (..),
  WriteAPI (..),
  ReadAPI (..),
) where

import Cavefish.Api.ServerConfiguration (ServerConfiguration (ServerConfiguration, httpServer, serviceProviderFee, transactionExpiry, wbps))
import Cavefish.Endpoints.Read.FetchAccount qualified as FetchAccount (Inputs, Outputs)
import Cavefish.Endpoints.Read.FetchAccounts qualified as FetchAccounts (Outputs)
import Cavefish.Endpoints.Write.AskCommitmentProof qualified as AskCommitmentProof
import Cavefish.Endpoints.Write.DemonstrateCommitment qualified as DemonstrateCommitment (Inputs, Outputs)
import Cavefish.Endpoints.Write.Register qualified as Register (Inputs, Outputs)
import Cavefish.Services.TxBuilding (ServiceFee (ServiceFee, amount, paidTo))
import Control.Monad ((>=>))
import Cooked (InitialDistribution (InitialDistribution), Payable (Value), receives)
import Data.Default (Default (def))
import Data.Foldable (foldl')
import Network.HTTP.Client (defaultManagerSettings, managerResponseTimeout, newManager, responseTimeoutMicro)
import Network.Wai.Handler.Warp qualified as Warp
import Path (Dir, Path, Rel)
import Plutus.Script.Utils.Value (ada)
import PlutusLedgerApi.V3 qualified as Api
import Servant (Application, Proxy (Proxy), type (:<|>) ((:<|>)))
import Servant.Client (BaseUrl (BaseUrl))
import Servant.Client qualified as SC
import Sp.Emulator (mkServerContext)
import Sp.Middleware (errStatusTraceMiddleware)
import Sp.Server (Cavefish, mkServer)
import Test.Hspec (expectationFailure)
import WBPS.Core.Registration.Artefacts.Groth16.Setup (PublicVerificationContextAsJSON)
import WBPS.Core.Registration.Artefacts.Keys.Ed25519 (KeyPair, Wallet (Wallet, paymentAddress), generateWallet)
import WBPS.Core.Session.BlindSigning.Sign (BlindSignature, sign)
import WBPS.Core.Session.BlindSigning.ThetaStatement (ThetaStatement)
import WBPS.Core.Session.BlindSigning.VerifyProof qualified as VerifyProof
import WBPS.Core.Session.Demonstration.Artefacts.R (RSecret)
import WBPS.Core.Session.Proving.Artefacts.Challenge (Challenge)
import WBPS.Core.Session.Proving.Artefacts.Proof (Proof)
import WBPS.Core.Setup.Circuit.FileScheme (FileScheme, mkFileSchemeFromRoot)
import WBPS.WBPS (runWBPS)

getServiceProviderAPI :: ServiceFee -> Int -> IO ServiceProviderAPI
getServiceProviderAPI fee port = do
  manager <- newManager defaultManagerSettings {managerResponseTimeout = responseTimeoutMicro 300_000_000}
  let baseUrl = BaseUrl SC.Http "127.0.0.1" port ""
      ( register
          :<|> demonstrateCommitment
          :<|> askCommitmentProof
          :<|> fetchAccount
          :<|> fetchAccounts
        ) = SC.client (Proxy @Cavefish)
  return
    ServiceProviderAPI
      { fee = fee
      , write =
          WriteAPI
            { register = runClientOrFail (SC.mkClientEnv manager baseUrl) . register
            , demonstrateCommitment = runClientOrFail (SC.mkClientEnv manager baseUrl) . demonstrateCommitment
            , askCommitmentProof = runClientOrFail (SC.mkClientEnv manager baseUrl) . askCommitmentProof
            }
      , read =
          ReadAPI
            { fetchAccount = runClientOrFail (SC.mkClientEnv manager baseUrl) . fetchAccount
            , fetchAccounts = runClientOrFail (SC.mkClientEnv manager baseUrl) fetchAccounts
            }
      }

data UserToolkitAPI
  = UserToolkitAPI
  { assertProofIsValid :: PublicVerificationContextAsJSON -> ThetaStatement -> Proof -> IO ()
  , signBlindly :: KeyPair -> RSecret -> Challenge -> IO BlindSignature
  }

data ServiceProviderAPI
  = ServiceProviderAPI
  { fee :: ServiceFee
  , write :: WriteAPI
  , read :: ReadAPI
  }

data WriteAPI = WriteAPI
  { register :: Register.Inputs -> IO Register.Outputs
  , demonstrateCommitment :: DemonstrateCommitment.Inputs -> IO DemonstrateCommitment.Outputs
  , askCommitmentProof :: AskCommitmentProof.Inputs -> IO AskCommitmentProof.Outputs
  }

data ReadAPI = ReadAPI
  { fetchAccount :: FetchAccount.Inputs -> IO FetchAccount.Outputs
  , fetchAccounts :: IO FetchAccounts.Outputs
  }

runClientOrFail :: SC.ClientEnv -> SC.ClientM a -> IO a
runClientOrFail clientEnv action = do
  SC.runClientM action clientEnv
    >>= \case
      Left err -> expectationFailure ("HTTP client call failed: " <> show err) >> fail "http client failure"
      Right value -> pure value

mkTestCavefishMonad ::
  FileScheme ->
  InitialDistribution ->
  ServerConfiguration ->
  Application
mkTestCavefishMonad wbpsScheme initialDistribution serverConfiguration =
  mkServer errStatusTraceMiddleware $
    mkServerContext
      initialDistribution
      wbpsScheme
      serverConfiguration

setupCavefish :: Path Rel Dir -> (Setup -> IO a) -> IO a
setupCavefish folderLabel actions = do
  wbpsScheme <- mkFileSchemeFromRoot folderLabel
  alice <- generateWallet
  bob <- generateWallet
  provider@Wallet {paymentAddress} <- generateWallet
  let servicefee = ServiceFee {amount = 10_000_000, paidTo = paymentAddress}
      userToolkit =
        UserToolkitAPI
          { assertProofIsValid = \publicVerificationContext statement proof ->
              runWBPS wbpsScheme (VerifyProof.assertProofIsValid publicVerificationContext statement proof)
                >>= \case
                  Left err ->
                    expectationFailure ("Proof verification failed: " <> show err)
                      >> fail "proof verification failed"
                  Right _ -> pure ()
          , signBlindly = \keyPair rSecret challenge ->
              runWBPS wbpsScheme (sign keyPair rSecret challenge)
                >>= \case
                  Left err ->
                    expectationFailure ("Blind signing failed: " <> show err)
                      >> fail "blind signing failed"
                  Right signature -> pure signature
          }
  Warp.testWithApplication
    ( pure $
        mkTestCavefishMonad
          wbpsScheme
          ( distributionFromList
              [ (alice, [ada 100])
              , (bob, [ada 200])
              , (provider, [ada 10])
              ]
          )
          ServerConfiguration
            { httpServer = def
            , wbps = def
            , serviceProviderFee = servicefee
            , transactionExpiry = def
            }
    )
    (getServiceProviderAPI servicefee >=> \serviceProvider -> actions Setup {..})

distributionFromList :: [(Wallet, [Api.Value])] -> Cooked.InitialDistribution
distributionFromList =
  InitialDistribution
    . foldl' (\x (user, values) -> x <> map (receives user . Value) values) []

data Setup = Setup
  { serviceProvider :: ServiceProviderAPI
  , userToolkit :: UserToolkitAPI
  , alice :: Wallet
  , bob :: Wallet
  , provider :: Wallet
  }
