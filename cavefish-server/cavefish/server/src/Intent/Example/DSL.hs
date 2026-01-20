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
  toCanonicalIntent,
  satisfies,
  outExactly,
  valuePositive,
) where

import Cardano.Api (FromJSON, ToJSON, Value)
import Cardano.Api qualified as Api
import Control.Applicative (Alternative ((<|>)))
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Default (Default (def))
import Data.Foldable (fold)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import GHC.IsList qualified
import GHC.List qualified
import Ledger (
  Slot,
  cardanoPubKeyHash,
 )
import Plutus.Script.Utils.Address (
  ToAddress (toAddress),
  ToPubKeyHash (toPubKeyHash),
 )
import PlutusLedgerApi.V1.Interval (
  Extended (Finite),
  Interval (Interval),
  LowerBound (LowerBound),
  UpperBound (UpperBound),
 )
import WBPS.Core.Session.Demonstration.Artefacts.Cardano.UnsignedTx (AbstractUnsignedTx, abstractTxUnsigned, txUnsigned)

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
  deriving (Eq, Show)

instance Semigroup CanonicalIntent where
  CanonicalIntent _spendFrom _payTo _mustMint _changeTo _maxFee _maxInterval
    <> CanonicalIntent _spendFrom' _payTo' _mustMint' _changeTo' _maxFee' _maxInterval' =
      CanonicalIntent
        (_spendFrom <> _spendFrom')
        (_payTo <> _payTo')
        (_mustMint <> _mustMint')
        (_changeTo' <|> _changeTo)
        (min _maxFee _maxFee')
        (min _maxInterval _maxInterval')

instance Monoid CanonicalIntent where
  mempty = def

instance Default CanonicalIntent where
  def = CanonicalIntent def def def def def def

type EvalDSL a = ExceptT Text (Writer CanonicalIntent) a

runDSLTransformer :: IntentDSL -> (Either Text (), CanonicalIntent)
runDSLTransformer dsl = runWriter (runExceptT (evalIntentDSL dsl))

toCanonicalIntent :: IntentDSL -> Either Text CanonicalIntent
toCanonicalIntent dsl = case runDSLTransformer dsl of
  (Left e, _) -> Left e
  (Right (), intent) -> Right intent

evalIntentDSL :: IntentDSL -> EvalDSL ()
evalIntentDSL dsl = case dsl of
  MustMintW v -> tell mempty {mustMint = [v]}
  SpendFromW w@(AddressW walletAddr) -> case parseAddr w of
    Nothing ->
      throwError $ "invalid address : " <> walletAddr
    Just addr -> tell mempty {spendFrom = [addr]}
  MaxIntervalW w -> tell mempty {maxInterval = Just $ fromInteger w}
  PayToW value w@(AddressW walletAddr) -> case parseAddr w of
    Nothing ->
      throwError $ "invalid address : " <> walletAddr
    Just addr -> tell mempty {payTo = [(value, addr)]}
  ChangeToW a -> tell mempty {changeTo = parseAddr a}
  MaxFeeW i -> tell mempty {maxFee = Just i}
  AndExpsW ys -> mapM_ evalIntentDSL ys
  where
    parseAddr :: AddressW -> Maybe AdressConwayEra
    parseAddr (AddressW addr) =
      fmap
        AdressConwayEra
        (Api.deserialiseAddress (Api.AsAddressInEra Api.AsConwayEra) addr)

-- |
-- The function that checks that a given abstract transaction matches the intent specification
-- See Section 5.1, sentence 3.69 for detail
satisfies :: IntentDSL -> AbstractUnsignedTx -> Bool
satisfies dsl txAbs =
  case toCanonicalIntent dsl of
    Left _ -> False
    Right intent ->
      and
        [ satisfiesMint intent (Api.txMintValue txBody)
        , satisfiesPayTo intent (Api.txOuts txBody)
        , satisfiesSpendFrom
        , satisfiesMaxFee intent (Api.txFee txBody)
        ]
  where
    txBody = Api.getTxBodyContent (txUnsigned . abstractTxUnsigned $ txAbs)
    txouts = Api.txOuts txBody

    satisfiesSpendFrom = True -- TODO
    satisfiesMaxFee :: CanonicalIntent -> Api.TxFee era -> Bool
    satisfiesMaxFee CanonicalIntent {maxFee} (Api.TxFeeExplicit _ (Api.Coin coin)) =
      maybe
        True
        (== coin)
        maxFee

    satisfiesPayTo :: CanonicalIntent -> [Api.TxOut Api.CtxTx Api.ConwayEra] -> Bool
    satisfiesPayTo CanonicalIntent {payTo} txOuts =
      all (\(value, addr) -> any (outExactly addr value) txOuts) payTo

    satisfiesMint :: CanonicalIntent -> Api.TxMintValue b e -> Bool
    satisfiesMint intent Api.TxMintNone = GHC.List.null $ mustMint intent
    satisfiesMint intent (Api.TxMintValue era policyIds) =
      let
        intentValueAssets :: Map.Map Api.AssetId Api.Quantity
        intentValueAssets = Map.fromList $ concatMap GHC.IsList.toList (mustMint intent)

        -- policyAssets:: [Api.PolicyAssets _ _]
        policyAssets :: [Api.PolicyAssets] = (fst <$> Map.elems policyIds)
        (Api.Quantity toMint) =
          foldMap (fold . (\(Api.PolicyAssets m) -> Map.elems m)) policyAssets
       in
        undefined

    need :: CanonicalIntent -> Map Api.AssetId Api.Quantity
    need = Map.fromList . GHC.IsList.toList . mconcat . mustMint
    -- have = Map.fromList (toList tx.absMint)

    satisfiesMaxInterval ::
      CanonicalIntent ->
      Api.TxBodyContent build era ->
      Bool
    satisfiesMaxInterval
      CanonicalIntent {maxInterval}
      Api.TxBodyContent {Api.txValidityLowerBound, Api.txValidityUpperBound} = True

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
    (Map.fromList (GHC.IsList.toList need))
    (Map.fromList (GHC.IsList.toList have))

valueEq :: Api.Value -> Api.Value -> Bool
valueEq a b = valueLeq a b && valueLeq b a

valuePositive :: Api.Value -> Bool
valuePositive = any (\(_, Api.Quantity q) -> q > 0) . GHC.IsList.toList

outExactly :: AdressConwayEra -> Api.Value -> Api.TxOut Api.CtxTx Api.ConwayEra -> Bool
outExactly addrReq vReq (Api.TxOut addr vOut _ _) =
  addrReq == AdressConwayEra addr && valueLeq vReq (Api.txOutValueToValue vOut)

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
