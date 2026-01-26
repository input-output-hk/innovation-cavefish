-- | Module      : Intent.Example.DSL
--   Description : Core definitions for transaction intents.
--     This module defines the data structures and functions related to transaction
--     intents, including their representation, normalization, and satisfaction checks
--     against built transactions.
module Intent.Example.DSL (
  CanonicalIntent (..),
  IntentDSL (..),
  AddressW (..),
  AdressConwayEra (..),
  ChangeDelta,
  emptyIntent,
  normalizeIntent,
  toIntentExpr,
  toCanonicalIntent,
  satisfies,
  outExactly,
  valuePositive,
) where

import Cardano.Api (FromJSON, ToJSON, Value)
import Cardano.Api qualified as Api
import Data.List.NonEmpty (NonEmpty)
import Data.Map qualified as Map
import Data.Text (Text)
import GHC.Exts (IsList (toList))
import GHC.Generics (Generic)
import Ledger (
  Slot,
  cardanoPubKeyHash,
 )
import Plutus.Script.Utils.Address (
  ToAddress (toAddress),
  ToPubKeyHash (toPubKeyHash),
 )
import WBPS.Core.Session.Steps.Demonstration.Artefacts.Cardano.UnsignedTx (AbstractUnsignedTx)

type ChangeDelta = Api.Value

newtype AdressConwayEra = AdressConwayEra {unAdressConwayEra :: Api.AddressInEra Api.ConwayEra}
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance ToAddress AdressConwayEra where
  toAddress = toAddress . toPubKeyHash

instance ToPubKeyHash AdressConwayEra where
  toPubKeyHash (AdressConwayEra addr) =
    case cardanoPubKeyHash addr of
      Just pkh -> pkh
      Nothing -> error "toPubKeyHash: address is not backed by a payment key hash"

-- Result of building a transaction

-- Wallet address as represented in the API
newtype AddressW = AddressW Text
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- Intent as represented in the API
data IntentDSL
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
  | AndExpsW (NonEmpty IntentDSL)
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
data InternalIntentDSL
  = -- | Must mint this value
    MustMint Value
  | -- | Spend from this source
    SpendFrom AdressConwayEra
  | -- | Maximum validity interval in slots
    MaxInterval Slot
  | -- | Pay this value to this address
    PayTo Value AdressConwayEra
  | -- | Send change to this address
    ChangeTo AdressConwayEra
  | -- | Maximum fee
    MaxFee Integer
  | AndExps (NonEmpty InternalIntentDSL)
  deriving (Eq, Show, Generic)

data CanonicalIntent = CanonicalIntent
  { spendFrom :: [AdressConwayEra]
  -- ^ Spend from these sources
  , payTo :: [(Api.Value, AdressConwayEra)]
  -- ^ Pay these values to these addresses
  , mustMint :: [Api.Value]
  -- ^ Must mint these values
  , changeTo :: Maybe AdressConwayEra
  -- ^ Send change to this address
  , maxFee :: Maybe Integer
  -- ^ Maximum fee
  , maxInterval :: Maybe Slot
  -- ^ Maximum validity interval in slots
  }
  deriving (Show)

emptyIntent :: CanonicalIntent
emptyIntent =
  CanonicalIntent
    { spendFrom = []
    , payTo = []
    , mustMint = []
    , changeTo = Nothing
    , maxFee = Nothing
    , maxInterval = Nothing
    }

-- | Normalize an InternalIntentDSL into a Canonical Intent
normalizeIntent :: InternalIntentDSL -> CanonicalIntent
normalizeIntent = go emptyIntent
  where
    go acc = \case
      MustMint v -> acc {mustMint = v : mustMint acc}
      SpendFrom s -> acc {spendFrom = s : spendFrom acc}
      MaxInterval i -> acc {maxInterval = Just (maybe i (min i) (maxInterval acc))}
      PayTo v a -> acc {payTo = (v, a) : payTo acc}
      ChangeTo a -> acc {changeTo = Just a}
      MaxFee f -> acc {maxFee = Just (maybe f (min f) (maxFee acc))}
      AndExps xs -> foldl go acc xs

toIntentExpr :: IntentDSL -> Either Text InternalIntentDSL
toIntentExpr = \case
  MustMintW v -> Right (MustMint v)
  SpendFromW walletAddr -> SpendFrom <$> parseAddr walletAddr
  MaxIntervalW w -> Right (MaxInterval (fromInteger w))
  PayToW v a -> PayTo v <$> parseAddr a
  ChangeToW a -> ChangeTo <$> parseAddr a
  MaxFeeW i -> Right (MaxFee i)
  AndExpsW xs -> AndExps <$> traverse toInt xs
  where
    parseAddr :: AddressW -> Either Text AdressConwayEra
    parseAddr (AddressW addr) =
      maybe
        (Left ("invalid address : " <> addr))
        (Right . AdressConwayEra)
        (Api.deserialiseAddress (Api.AsAddressInEra Api.AsConwayEra) addr)
    toInt :: IntentDSL -> Either Text InternalIntentDSL
    toInt = \case
      MustMintW v -> Right (MustMint v)
      SpendFromW walletAddr -> SpendFrom <$> parseAddr walletAddr
      MaxIntervalW w -> Right (MaxInterval (fromInteger w))
      PayToW v a -> PayTo v <$> parseAddr a
      ChangeToW a -> ChangeTo <$> parseAddr a
      MaxFeeW i -> Right (MaxFee i)
      AndExpsW ys -> AndExps <$> traverse toInt ys

toCanonicalIntent :: IntentDSL -> Either Text CanonicalIntent
toCanonicalIntent :: IntentDSL -> Either Text CanonicalIntent =
  fmap normalizeIntent . toIntentExpr

satisfies :: IntentDSL -> AbstractUnsignedTx -> Bool
satisfies _ _ = True

-- and
--   [ -- MustMint: v ≤ tx.mint
--     let need = Map.fromList . toList $ mconcat mustMint
--         have = Map.fromList (toList tx.absMint)
--      in Map.isSubmapOfBy (<=) need have
--   , -- SpendFrom: s(dom (tx.sigs), tx.validityInterval)
--     -- TODO WG: Not really sure how to do this right now (in a way that's fully coherent)
--     all (hasSigner tx.sigKeys . unAdressConwayEra) spendFrom
--   , -- MaxInterval (if any): (tx.validityInterval.snd - tx.validityInterval.fst) ≤ i
--     case maxInterval of
--       Nothing -> True
--       Just i -> case toClosedFinite tx.validityInterval of
--         Nothing -> False
--         Just (lo, hi) -> (hi - lo) <= i
--   , -- PayTo: (s, v) ∈ tx.outputs
--     all (\(value, addr) -> any (outMatches addr value) tx.outputs) payTo
--   , -- ChangeTo: (s,consumed − produced) ∈ tx.outputs
--     case changeTo of
--       Nothing -> True
--       Just addr -> any (outMatchesChange addr) tx.outputs
--   , -- not (valuePositive cd) || any (outMatchesChange addr) tx.outputs
--     -- MaxFee (if any): tx.fee ≤ f
--     maybe True (\f -> tx.absFee <= f) maxFee
--   ]
-- where
--   hasSigner :: Set.Set PubKey -> Api.AddressInEra Api.ConwayEra -> Bool
--   hasSigner sigs addr =
--     case cardanoPubKeyHash addr of
--       Nothing -> False
--       Just pkh -> any ((== pkh) . pubKeyHash) (Set.toList sigs)

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

-- outMatches ::
--   AdressConwayEra ->
--   Api.Value ->
--   Api.TxOut Api.CtxTx Api.ConwayEra ->
--   Bool
-- outMatches addrReq vReq (Api.TxOut addr vOut _ _) =
--   addrReq == AdressConwayEra addr && valueLeq vReq (Api.txOutValueToValue vOut)

outExactly ::
  AdressConwayEra ->
  Api.Value ->
  Api.TxOut Api.CtxTx Api.ConwayEra ->
  Bool
outExactly addrReq vReq (Api.TxOut addr vOut _ _) =
  addrReq == AdressConwayEra addr && valueEq vReq (Api.txOutValueToValue vOut)

-- outMatchesChange ::
--   AdressConwayEra ->
--   Api.TxOut Api.CtxTx Api.ConwayEra ->
--   Bool
-- outMatchesChange addrReq (Api.TxOut addr vOut _ _) =
--   addrReq == AdressConwayEra addr && valueIsPlaceholder (Api.txOutValueToValue vOut)

-- valueIsPlaceholder :: Api.Value -> Bool
-- valueIsPlaceholder = valueEq mempty

-- | Convert an interval to a closed finite interval, if possible.
-- toClosedFinite :: (Num a, Ord a) => Interval a -> Maybe (a, a)
-- toClosedFinite (Interval (LowerBound loE loC) (UpperBound hiE hiC)) = do
--   lo <- case loE of Finite x -> Just x; _ -> Nothing
--   hi <- case hiE of Finite x -> Just x; _ -> Nothing
--   let lo' = if loC then lo else lo + 1
--       hi' = if hiC then hi else hi - 1
--   if hi' >= lo' then Just (lo', hi') else Nothing
