-- | Module      : Core.Intent
--   Description : Core definitions for transaction intents.
--     This module defines the data structures and functions related to transaction
--     intents, including their representation, normalization, and satisfaction checks
--     against built transactions.
module Core.Intent where

import Cardano.Api (ConwayEra, FromJSON, ToJSON, Value)
import Cardano.Api qualified as Api
import Cooked (MockChainState)
import Core.TxAbs (TxAbs (absFee, absMint, outputs, sigKeys, validityInterval))
import Data.Foldable (foldl)
import Data.List.NonEmpty (NonEmpty)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import GHC.Exts (IsList (toList))
import GHC.Generics (Generic)
import Ledger (
  Extended (Finite),
  Interval (Interval),
  LowerBound (LowerBound),
  PubKey,
  Slot,
  UpperBound (UpperBound),
  cardanoPubKeyHash,
  pubKeyHash,
 )

type ChangeDelta = Api.Value

-- Spend source address
newtype Spend = Spend {source :: Api.AddressInEra Api.ConwayEra}
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- Result of building a transaction
data BuildTxResult = BuildTxResult
  { tx :: Api.Tx Api.ConwayEra
  -- ^ Built transaction
  , changeDelta :: ChangeDelta
  -- ^ Change delta
  , txAbs :: TxAbs Api.ConwayEra
  -- ^ Abstract representation of the transaction
  , mockState :: MockChainState
  -- ^ Updated mock chain state
  }
  deriving (Show, Generic)

-- Wallet address as represented in the API
newtype AddressW = AddressW Text
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- Intent as represented in the API
data IntentW
  = -- | Must mint this value
    MustMintW Value
  | -- | Spend from this wallet address
    SpendFromW AddressW
  | -- | Maximum validity interval in slots
    MaxIntervalW Integer
  | -- | Pay this value to this address
    PayToW Value AddressW
  | -- | Send change to this address
    ChangeToW AddressW
  | -- | Maximum fee
    MaxFeeW Integer
  | AndExpsW (NonEmpty IntentW)
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

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
  = -- | Must mint this value
    MustMint Value
  | -- | Spend from this source
    SpendFrom Spend
  | -- | Maximum validity interval in slots
    MaxInterval Slot
  | -- | Pay this value to this address
    PayTo Value (Api.AddressInEra ConwayEra)
  | -- | Send change to this address
    ChangeTo (Api.AddressInEra ConwayEra)
  | -- | Maximum fee
    MaxFee Integer
  | AndExps (NonEmpty IntentExpr)
  deriving (Eq, Show, Generic)

data Intent = Intent
  { irSpendFrom :: [Spend]
  -- ^ Spend from these sources
  , irPayTo :: [(Api.Value, Api.AddressInEra Api.ConwayEra)]
  -- ^ Pay these values to these addresses
  , irMustMint :: [Api.Value]
  -- ^ Must mint these values
  , irChangeTo :: Maybe (Api.AddressInEra Api.ConwayEra)
  -- ^ Send change to this address
  , irMaxFee :: Maybe Integer
  -- ^ Maximum fee
  , irMaxInterval :: Maybe Slot
  -- ^ Maximum validity interval in slots
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

-- | Normalize an IntentExpr into an Intent
normalizeIntent :: IntentExpr -> Intent
normalizeIntent = go emptyIntent
  where
    go acc = \case
      MustMint v -> acc {irMustMint = v : irMustMint acc}
      SpendFrom s -> acc {irSpendFrom = s : irSpendFrom acc}
      MaxInterval i -> acc {irMaxInterval = Just (maybe i (min i) (irMaxInterval acc))}
      PayTo v a -> acc {irPayTo = (v, a) : irPayTo acc}
      ChangeTo a -> acc {irChangeTo = Just a}
      MaxFee f -> acc {irMaxFee = Just (maybe f (min f) (irMaxFee acc))}
      AndExps xs -> Data.Foldable.foldl go acc xs

toIntentExpr :: IntentW -> Either Text IntentExpr
toIntentExpr = \case
  MustMintW v -> Right (MustMint v)
  SpendFromW walletAddr -> fmap SpendFrom $ Spend <$> parseAddr walletAddr
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
      SpendFromW walletAddr -> fmap SpendFrom $ Spend <$> parseAddr walletAddr
      MaxIntervalW w -> Right (MaxInterval (fromInteger w))
      PayToW v a -> PayTo v <$> parseAddr a
      ChangeToW a -> ChangeTo <$> parseAddr a
      MaxFeeW i -> Right (MaxFee i)
      AndExpsW ys -> AndExps <$> traverse toInt ys

toInternalIntent :: IntentW -> Either Text Intent
toInternalIntent = fmap normalizeIntent . toIntentExpr

-- | Check whether a transaction satisfies an intent, given the change delta.
satisfies :: ChangeDelta -> Intent -> TxAbs Api.ConwayEra -> Bool
satisfies cd Intent {..} tx =
  and
    [ -- MustMint: v ≤ tx.mint
      let need = Map.fromList . toList $ mconcat irMustMint
          have = Map.fromList (toList tx.absMint)
       in Map.isSubmapOfBy (<=) need have
    , -- SpendFrom: s(dom (tx.sigs), tx.validityInterval)
      -- TODO WG: Not really sure how to do this right now (in a way that's fully coherent)
      all (hasSigner tx.sigKeys . source) irSpendFrom
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
        Just addr ->
          not (valuePositive cd) || any (outMatchesChange addr) tx.outputs
    , -- MaxFee (if any): tx.fee ≤ f
      maybe True (\f -> tx.absFee <= f) irMaxFee
    ]
  where
    hasSigner :: Set.Set PubKey -> Api.AddressInEra Api.ConwayEra -> Bool
    hasSigner sigs addr =
      case cardanoPubKeyHash addr of
        Nothing -> False
        Just pkh -> any ((== pkh) . pubKeyHash) (Set.toList sigs)

valueLeq :: Api.Value -> Api.Value -> Bool
valueLeq need have =
  Map.isSubmapOfBy
    (<=)
    (Map.fromList (toList need))
    (Map.fromList (toList have))

valueEq :: Api.Value -> Api.Value -> Bool
valueEq a b = valueLeq a b && valueLeq b a

valuePositive :: Api.Value -> Bool
valuePositive = any (\(_, Api.Quantity q) -> q > 0) . toList

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

outMatchesChange ::
  Api.AddressInEra Api.ConwayEra ->
  Api.TxOut Api.CtxTx Api.ConwayEra ->
  Bool
outMatchesChange addrReq (Api.TxOut addr vOut _ _) =
  addrReq == addr && valueIsPlaceholder (Api.txOutValueToValue vOut)

valueIsPlaceholder :: Api.Value -> Bool
valueIsPlaceholder = valueEq mempty

-- | Convert an interval to a closed finite interval, if possible.
toClosedFinite :: (Num a, Ord a) => Interval a -> Maybe (a, a)
toClosedFinite (Interval (LowerBound loE loC) (UpperBound hiE hiC)) = do
  lo <- case loE of Finite x -> Just x; _ -> Nothing
  hi <- case hiE of Finite x -> Just x; _ -> Nothing
  let lo' = if loC then lo else lo + 1
      hi' = if hiC then hi else hi - 1
  if hi' >= lo' then Just (lo', hi') else Nothing
