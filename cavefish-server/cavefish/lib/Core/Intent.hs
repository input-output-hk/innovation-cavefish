module Core.Intent where

import Cardano.Api hiding (Env)
import Cardano.Api qualified as Api
import Cooked (MockChainState)
import Cooked.Wallet (Wallet)
import Core.TxAbs (TxAbs (..))
import Crypto.PubKey.Ed25519 (SecretKey)
import Data.ByteString (ByteString)
import Data.Foldable (find, foldl)
import Data.List.NonEmpty (NonEmpty)
import Data.Map as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime)
import GHC.Generics
import Ledger (Extended (..), Interval (..), LowerBound (..), PubKey, Slot, UpperBound (..))
import Sp.State (PendingStore)

type ChangeDelta = Api.Value

data BuildTxResult = BuildTxResult
  { tx :: Api.Tx Api.ConwayEra
  , changeDelta :: ChangeDelta
  , txAbs :: TxAbs Api.ConwayEra
  , mockState :: MockChainState
  }
  deriving (Show, Generic)

-- Shouldn't really be in this file
data Env = Env
  { spSk :: SecretKey
  , pending :: PendingStore
  , ttl :: NominalDiffTime
  , spWallet :: Wallet
  , resolveWallet :: Api.AddressInEra Api.ConwayEra -> Maybe Wallet
  , spFee :: Integer
  , build ::
      Intent ->
      ByteString ->
      IO BuildTxResult
  , submit ::
      Api.Tx Api.ConwayEra ->
      MockChainState ->
      IO (Either Text ())
  , scriptReg :: [ScriptSpec]
  -- TODO WG: Add a registry of public keys for LCs (and, come to think of it, an endpoint for registration).
  --          That way, we can verify the signature in `finaliseH`
  }

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
  | AndExpsW (NonEmpty IntentW)
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ScriptInputs = ScriptInputs (Set.Set PubKey) (Interval Slot) deriving (Eq, Show, Generic)

data ScriptSpec = ScriptSpec
  { ssId :: Api.AddressInEra ConwayEra
  , ssEval :: ScriptInputs -> Bool
  }

lookupScriptByAddr :: [ScriptSpec] -> Api.AddressInEra ConwayEra -> Maybe ScriptSpec
lookupScriptByAddr scriptRegistry id' =
  find (\ScriptSpec{..} -> ssId == id') scriptRegistry

{- Design:
    The SP depends upon a mapping from Intent -> Transaction,
    and the client depends upon a mapping from Intent -> Observer

    Also, intents seem to be split between directive and constraint:

    Directive:
      MustMint
      SpendFrom
      PayTo
      ChangeTo

    Constraint:
      MaxInterval
      MaxFee

    We don't really need this type anymore, but I'm keeping it around for parity with the paper.
 -}
data IntentExpr
  = MustMint Value
  | SpendFrom (Api.AddressInEra ConwayEra)
  | MaxInterval Slot
  | PayTo Value (Api.AddressInEra ConwayEra)
  | ChangeTo (Api.AddressInEra ConwayEra)
  | MaxFee Integer
  | AndExps (NonEmpty IntentExpr)
  deriving (Eq, Show, Generic)

data Intent = Intent
  { irSpendFrom :: [Api.AddressInEra Api.ConwayEra]
  , irPayTo :: [(Api.Value, Api.AddressInEra Api.ConwayEra)]
  , irMustMint :: [Api.Value]
  , irChangeTo :: Maybe (Api.AddressInEra Api.ConwayEra)
  , irMaxFee :: Maybe Integer
  , irMaxInterval :: Maybe Slot
  }

emptyIntent :: Intent
emptyIntent =
  Intent
    { irSpendFrom = []
    , irPayTo = []
    , irMustMint = []
    , irChangeTo = Nothing
    , irMaxFee = Nothing
    , irMaxInterval = Nothing
    }

normalizeIntent :: IntentExpr -> Intent
normalizeIntent = go emptyIntent
 where
  go acc = \case
    MustMint v -> acc{irMustMint = v : irMustMint acc}
    SpendFrom a -> acc{irSpendFrom = a : irSpendFrom acc}
    MaxInterval i -> acc{irMaxInterval = Just (maybe i (min i) (irMaxInterval acc))}
    PayTo v a -> acc{irPayTo = (v, a) : irPayTo acc}
    ChangeTo a -> acc{irChangeTo = Just a}
    MaxFee f -> acc{irMaxFee = Just (maybe f (min f) (irMaxFee acc))}
    AndExps xs -> Data.Foldable.foldl go acc xs

toIntentExpr :: IntentW -> Either Text IntentExpr
toIntentExpr = \case
  MustMintW v -> Right (MustMint v)
  SpendFromW addr -> SpendFrom <$> parseAddr addr
  MaxIntervalW w -> Right (MaxInterval (fromInteger w))
  PayToW v a -> PayTo v <$> parseAddr a
  ChangeToW a -> ChangeTo <$> parseAddr a
  MaxFeeW i -> Right (MaxFee i)
  AndExpsW xs -> AndExps <$> traverse toInt xs
 where
  parseAddr :: AddressW -> Either Text (Api.AddressInEra Api.ConwayEra)
  parseAddr (AddressW addr) =
    maybe
      (Left "invalid address")
      Right
      (Api.deserialiseAddress (Api.AsAddressInEra Api.AsConwayEra) addr)
  toInt :: IntentW -> Either Text IntentExpr
  toInt = \case
    MustMintW v -> Right (MustMint v)
    SpendFromW addr -> SpendFrom <$> parseAddr addr
    MaxIntervalW w -> Right (MaxInterval (fromInteger w))
    PayToW v a -> PayTo v <$> parseAddr a
    ChangeToW a -> ChangeTo <$> parseAddr a
    MaxFeeW i -> Right (MaxFee i)
    AndExpsW ys -> AndExps <$> traverse toInt ys

toInternalIntent :: IntentW -> Either Text Intent
toInternalIntent = fmap normalizeIntent . toIntentExpr

satisfies :: Env -> ChangeDelta -> Intent -> TxAbs Api.ConwayEra -> Bool
satisfies env cd Intent{..} tx =
  and
    [ -- MustMint: v ≤ tx.mint
      let need = Map.fromList . valueToList $ mconcat irMustMint
          have = Map.fromList (valueToList tx.absMint)
       in Map.isSubmapOfBy (<=) need have
    , -- SpendFrom: s(dom (tx.sigs), tx.validityInterval)
      all
        ( \h ->
            -- TODO WG: Realism - Proper script lookup - Can we do that with `cooked`?
            case lookupScriptByAddr env.scriptReg h of
              Nothing -> False
              Just ScriptSpec{..} -> ssEval (ScriptInputs tx.sigKeys tx.validityInterval)
        )
        irSpendFrom
    , -- MaxInterval (if any): (tx.validityInterval.snd - tx.validityInterval.fst) ≤ i
      case irMaxInterval of
        Nothing -> True
        Just i -> case toClosedFinite tx.validityInterval of
          Nothing -> False
          Just (lo, hi) -> (hi - lo) <= i
    , -- PayTo: (s, v) ∈ tx.outputs
      all (\(value, addr) -> any (outMatches addr value) tx.outputs) irPayTo
    , -- ChangeTo: (s,consumed − produced) ∈ tx.outputs
      case irChangeTo of
        Nothing -> True
        Just addr -> if valuePositive cd then any (outExactly addr cd) tx.outputs else True
    , -- MaxFee (if any): tx.fee ≤ f
      maybe True (\f -> tx.absFee <= f) irMaxFee
    ]

valueLeq :: Api.Value -> Api.Value -> Bool
valueLeq need have =
  Map.isSubmapOfBy
    (<=)
    (Map.fromList (Api.valueToList need))
    (Map.fromList (Api.valueToList have))

valueEq :: Api.Value -> Api.Value -> Bool
valueEq a b = valueLeq a b && valueLeq b a

valuePositive :: Api.Value -> Bool
valuePositive = any (\(_, Api.Quantity q) -> q > 0) . Api.valueToList

outMatches ::
  Api.AddressInEra Api.ConwayEra ->
  Api.Value ->
  Api.TxOut Api.CtxTx Api.ConwayEra ->
  Bool
outMatches addrReq vReq (Api.TxOut addr vOut _ _) =
  addrReq == addr && valueLeq vReq (Api.txOutValueToValue vOut)

outExactly ::
  Api.AddressInEra Api.ConwayEra ->
  Api.Value ->
  Api.TxOut Api.CtxTx Api.ConwayEra ->
  Bool
outExactly addrReq vReq (Api.TxOut addr vOut _ _) =
  addrReq == addr && valueEq vReq (Api.txOutValueToValue vOut)

toClosedFinite :: (Num a, Ord a) => Interval a -> Maybe (a, a)
toClosedFinite (Interval (LowerBound loE loC) (UpperBound hiE hiC)) = do
  lo <- case loE of Finite x -> Just x; _ -> Nothing
  hi <- case hiE of Finite x -> Just x; _ -> Nothing
  let lo' = if loC then lo else lo + 1
      hi' = if hiC then hi else hi - 1
  if hi' >= lo' then Just (lo', hi') else Nothing
