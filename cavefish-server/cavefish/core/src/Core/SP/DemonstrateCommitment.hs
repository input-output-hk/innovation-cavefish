module Core.SP.DemonstrateCommitment (handle, Inputs (..), Outputs (..), Commitment (..)) where

import Cardano.Api qualified as Api
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader (ask))
import Core.Api.AppContext (AppM, Env (Env, build, wbpsScheme))
import Core.Intent (
  BuildTxResult (BuildTxResult, tx),
  IntentDSL,
 )
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy.Char8 qualified as BL8
import GHC.Generics (Generic)
import Servant (
  err500,
  errBody,
  throwError,
 )
import WBPS.Commitment (
  Commitment,
  PublicMessage (PublicMessage),
  Session (SessionCreated, commitment, publicMessage),
 )
import WBPS.Commitment qualified as WBPS
import WBPS.Core.Keys.Ed25519 (UserWalletPublicKey)
import WBPS.Registration qualified as WBPS

data Inputs = Inputs
  { userWalletPublicKey :: UserWalletPublicKey
  , intent :: IntentDSL
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

data Outputs = Outputs
  { commitment :: WBPS.Commitment.Commitment
  , txAbs :: Api.Tx Api.ConwayEra
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

handle :: Inputs -> AppM Outputs
handle Inputs {userWalletPublicKey, intent} = do
  Env {build, wbpsScheme} <- ask
  BuildTxResult {tx = tx} <- build intent

  liftIO (WBPS.withFileSchemeIO wbpsScheme (WBPS.createSession userWalletPublicKey tx))
    >>= \case
      (Left e) -> throwError err500 {errBody = BL8.pack ("Unexpected event" ++ show e)}
      ( Right
          (WBPS.Commitment.SessionCreated {publicMessage = WBPS.Commitment.PublicMessage txAbs, commitment})
        ) -> do
          return Outputs {txAbs, commitment}
