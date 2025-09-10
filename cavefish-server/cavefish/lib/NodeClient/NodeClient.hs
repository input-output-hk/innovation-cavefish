module NodeClient.NodeClient where

import Cardano.Api qualified as Api
import Cardano.Api.Network qualified as Network

import Control.Monad.Trans.Except (runExceptT)
import Data.Map.Strict (Map)
import Data.Function ((&))
import qualified Data.Set as Set
import Data.Text (Text)

connectInfo :: Api.LocalNodeConnectInfo
connectInfo =
  Api.LocalNodeConnectInfo
    { Api.localConsensusModeParams = Api.CardanoModeParams (Api.EpochSlots 500)
    , Api.localNodeNetworkId       = Api.Testnet (Api.NetworkMagic 42)
    , Api.localNodeSocketPath      = Api.File "/home/will/git/cardano-node/cluster/socket/node1/sock"
    }

getShelleyBasedEra :: IO Api.AnyShelleyBasedEra
getShelleyBasedEra = do
  eEra <- runExceptT $
            Api.queryNodeLocalState
              connectInfo
              Network.VolatileTip
              Api.QueryCurrentEra
  case eEra of
    Right (Api.AnyCardanoEra era) ->
      Api.caseByronOrShelleyBasedEra
        (fail "todo")
        (pure . Api.AnyShelleyBasedEra)
        era
    Left acquireErr ->
      fail $ "Could not acquire local state: " <> show acquireErr

utxoForAddress
  :: Api.LocalNodeConnectInfo
  -> Api.ShelleyBasedEra era
  -> Text
  -> IO (Either String (Api.UTxO era))
utxoForAddress lnc sbe addrText =
  case Api.deserialiseAddress Api.AsAddressAny addrText of
    Nothing   -> pure (Left "Invalid address")
    Just addr -> do
      let qInEra =
            Api.QueryInEra
              (Api.QueryInShelleyBasedEra sbe
                (Api.QueryUTxO (Api.QueryUTxOByAddress (Set.singleton addr))))
      r <- runExceptT $ Api.queryNodeLocalState lnc Api.VolatileTip qInEra
      pure $ case r of
        Left acqFail           -> Left $ "Acquire failure: " <> show acqFail
        Right (Left mismatch)  -> Left $ "Era mismatch: "    <> show mismatch
        Right (Right utxo)     -> Right utxo