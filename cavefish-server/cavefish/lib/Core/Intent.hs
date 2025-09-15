module Core.Intent where

import Cardano.Api
import Cardano.Api qualified as Api
import Core.TxAbs (TxAbs (..))
import Data.Foldable (find)
import Data.Map as Map
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Generics
import Ledger (Extended (..), Interval (..), LowerBound (..), PubKey, Slot, UpperBound (..))

newtype AddressW = AddressW Text
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data IntentW
  = MustMintW Value
  | SpendFromW AddressW
  | MaxIntervalW Integer
  | PayToW Value AddressW
  | ChangeToW AddressW
  | MaxFeeW Integer
  | AndExpsW [IntentW]
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- Mock address for WIP
data MockAddress = Addr1 | Addr2 deriving (Eq, Show, Ord, Generic)
data ScriptInputs = ScriptInputs (Set.Set PubKey) (Interval Slot) deriving (Eq, Show, Generic)

data ScriptSpec = ScriptSpec
  { ssId :: MockAddress
  , ssEval :: ScriptInputs -> Bool
  }

-- Dummy registry for WIP
scriptRegistry :: [ScriptSpec]
scriptRegistry = [payV1]

payV1 :: ScriptSpec
payV1 =
  ScriptSpec
    { ssId = Addr1
    , ssEval = const True
    }

lookupScriptByAddr :: MockAddress -> Maybe ScriptSpec
lookupScriptByAddr id' =
  find (\ScriptSpec{..} -> ssId == id') scriptRegistry

data Intent era
  = MustMint Value
  | SpendFrom MockAddress
  | MaxInterval Slot
  | PayTo Value (Api.AddressInEra ConwayEra)
  | ChangeTo (Api.AddressInEra ConwayEra)
  | MaxFee Integer
  | AndExps [Intent era]
  deriving (Eq, Show, Generic)

toInternalIntent :: IntentW -> Either Text (Intent ConwayEra)
toInternalIntent = \case
  MustMintW v -> Right (MustMint v)
  SpendFromW _sh -> Right (SpendFrom Addr1)
  MaxIntervalW w -> Right (MaxInterval (fromInteger w))
  PayToW v a -> PayTo v <$> parseAddr a
  ChangeToW a -> ChangeTo <$> parseAddr a
  MaxFeeW i -> Right (MaxFee i)
  AndExpsW xs -> AndExps <$> traverse toInternalIntent xs
 where
  parseAddr :: AddressW -> Either Text (Api.AddressInEra Api.ConwayEra)
  parseAddr (AddressW addr) =
    maybe
      (Left "invalid address")
      Right
      (Api.deserialiseAddress (Api.AsAddressInEra Api.AsConwayEra) addr)

satisfies :: Intent Api.ConwayEra -> TxAbs Api.ConwayEra -> Bool
satisfies ipost tx =
  case ipost of
    -- v ≤ tx.mint
    MustMint (Map.fromList . valueToList -> need) ->
      let have = Map.fromList (valueToList tx.txAbsMint)
       in Map.isSubmapOfBy (<=) need have
    -- s(dom (tx.sigs), tx.validityInterval)
    SpendFrom h ->
      case lookupScriptByAddr h of
        Nothing -> False
        Just ScriptSpec{..} ->
          ssEval (ScriptInputs tx.txAbsSigKeys tx.txAbsValidityInterval)
    -- (tx.validityInterval.snd - tx.validityInterval.fst) ≤ i
    MaxInterval i ->
      case toClosedFinite tx.txAbsValidityInterval of
        Nothing -> False
        Just (lo, hi) -> (hi - lo) <= i
    -- (s, v) ∈ tx.outputs
    PayTo value addr -> any (outMatches addr value) tx.txAbsOutputs
    -- (s,consumed − produced) ∈ tx.outputs
    ChangeTo _addr -> False -- TODO WG: Need more info to compute `consumed - produced`
    -- tx.fee ≤ f
    MaxFee fee -> tx.txAbsFee <= fee
    AndExps intents -> all (`satisfies` tx) intents

valueLeq :: Api.Value -> Api.Value -> Bool
valueLeq need have =
  Map.isSubmapOfBy
    (<=)
    (Map.fromList (Api.valueToList need))
    (Map.fromList (Api.valueToList have))

outMatches ::
  Api.AddressInEra Api.ConwayEra ->
  Api.Value ->
  Api.TxOut Api.CtxTx Api.ConwayEra ->
  Bool
outMatches addrReq vReq (Api.TxOut addr vOut _ _) =
  addrReq == addr && valueLeq vReq (Api.txOutValueToValue vOut)

toClosedFinite :: (Num a, Ord a) => Interval a -> Maybe (a, a)
toClosedFinite (Interval (LowerBound loE loC) (UpperBound hiE hiC)) = do
  lo <- case loE of Finite x -> Just x; _ -> Nothing
  hi <- case hiE of Finite x -> Just x; _ -> Nothing
  let lo' = if loC then lo else lo + 1
      hi' = if hiC then hi else hi - 1
  if hi' >= lo' then Just (lo', hi') else Nothing