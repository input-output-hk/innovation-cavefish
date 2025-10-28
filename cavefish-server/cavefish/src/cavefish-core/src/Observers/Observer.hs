{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- TODO WG: Necessary because of `cooked` versioning...is there a way around this?
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.0.0 #-}

-- | Module defining an ObserverScript that enforces transaction intents on-chain.
--   This module provides the data structures and logic to create a Plutus stake validator
--   script that checks whether a transaction satisfies a given intent, including spending
--   from specified credentials, paying to certain addresses, minting required values,
--   handling change, fee limits, and validity intervals.
module Observers.Observer where

import Cardano.Api qualified as Api
import Cardano.Api.Shelley (PlutusScript (..))
import Core.Intent (Intent (..), source)
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.ByteString.Short (fromShort, toShort)
import Data.Coerce (coerce)
import Data.Text (Text)
import GHC.Generics (Generic)
import Ledger.Address qualified as LedgerAddr
import Ledger.Slot (Slot (..))
import Ledger.Tx.CardanoAPI (fromCardanoPlutusScript)
import Ledger.Value.CardanoAPI qualified as LedgerValue
import Plutus.Script.Utils.Scripts (
  Language (PlutusV2),
  Script (..),
  StakeValidator (StakeValidator, getStakeValidator),
  ToStakeValidator (toStakeValidator),
  ToVersioned (..),
  Versioned (Versioned),
 )
import Plutus.Script.Utils.V2 (toCardanoScript)
import PlutusCore.Core (plcVersion100)
import PlutusLedgerApi.Common (BuiltinData)
import PlutusLedgerApi.V1.Value qualified as Value
import PlutusLedgerApi.V2 qualified as PV2
import PlutusTx qualified
import PlutusTx.Builtins.Internal qualified as BI
import PlutusTx.Builtins.Internal qualified as Builtins
import PlutusTx.Prelude qualified as P

newtype ObserverScript = ObserverScript
  { observerStakeValidator :: StakeValidator
  }
  deriving stock (Eq, Show, Generic)

data ObserverIntent = ObserverIntent
  { oiSpendFrom :: [PV2.Credential]
  , oiPayTo :: [(PV2.Value, PV2.Address)]
  , oiMustMint :: PV2.Value
  , oiChangeTo :: Maybe PV2.Address
  , oiMaxFee :: Maybe Integer
  , oiMaxInterval :: Maybe Integer
  }

-- Emulator slots are 1s long
slotLengthMillis :: Integer
slotLengthMillis = 1000

PlutusTx.unstableMakeIsData ''ObserverIntent

intentObserverScript :: Intent -> Either Text ObserverScript
intentObserverScript intent = mkObserverScript <$> toObserverIntent intent

mkObserverScript :: ObserverIntent -> ObserverScript
mkObserverScript = ObserverScript . mkStakeValidator

intentStakeValidatorBytes :: Intent -> Either Text ByteString
intentStakeValidatorBytes intent = observerScriptBytes <$> intentObserverScript intent

instance ToVersioned Script StakeValidator where
  toVersioned sv = Versioned (getStakeValidator sv) PlutusV2

stakeValidatorToBytes :: StakeValidator -> ByteString
stakeValidatorToBytes stakeValidator =
  case toCardanoScript (getStakeValidator stakeValidator) of
    Api.PlutusScript Api.PlutusScriptV2 (PlutusScriptSerialised scriptShort) -> fromShort scriptShort

stakeValidatorFromBytes :: ByteString -> StakeValidator
stakeValidatorFromBytes scriptBytes =
  let plutusScript = fromCardanoPlutusScript (PlutusScriptSerialised $ toShort scriptBytes)
      script :: Script
      script = coerce plutusScript
   in StakeValidator script

observerScriptBytes :: ObserverScript -> ByteString
observerScriptBytes =
  stakeValidatorToBytes . observerStakeValidator

-- TODO WG: HLS doesn't like Plutus it seems. Is there a way around needing an if def?

mkStakeValidator :: ObserverIntent -> StakeValidator
mkStakeValidator oi =
  toStakeValidator compiled
  where
    intentData = PlutusTx.toBuiltinData oi
    compiled =
      $$(PlutusTx.compile [||wrap||])
        `PlutusTx.unsafeApplyCode` PlutusTx.liftCode plcVersion100 intentData
    wrap :: BuiltinData -> BuiltinData -> BuiltinData -> P.BuiltinUnit
    wrap rawData redeemerRaw ctxRaw =
      let intent = PlutusTx.unsafeFromBuiltinData rawData
       in untypedObserverStakeValidator intent redeemerRaw ctxRaw

{-# INLINEABLE untypedObserverStakeValidator #-}
untypedObserverStakeValidator :: ObserverIntent -> BuiltinData -> BuiltinData -> P.BuiltinUnit
untypedObserverStakeValidator intent redeemerRaw ctxRaw =
  let redeemer :: ()
      redeemer = PlutusTx.unsafeFromBuiltinData redeemerRaw
      ctx = PlutusTx.unsafeFromBuiltinData ctxRaw
   in if observerStakeValidatorLogic intent redeemer ctx
        then Builtins.unitval
        else P.traceError "observer intent not satisfied"

observerStakeValidatorLogic :: ObserverIntent -> () -> PV2.ScriptContext -> Bool
{-# INLINEABLE observerStakeValidatorLogic #-}
observerStakeValidatorLogic ObserverIntent {oiSpendFrom, oiPayTo, oiMustMint, oiChangeTo, oiMaxFee, oiMaxInterval} _ ctx =
  case ctx of
    PV2.ScriptContext {PV2.scriptContextTxInfo = info} ->
      case info of
        PV2.TxInfo
          { PV2.txInfoInputs = inputs
          , PV2.txInfoOutputs = outputs
          , PV2.txInfoMint = mintedValue
          , PV2.txInfoSignatories = signers
          , PV2.txInfoFee = feeValue
          , PV2.txInfoValidRange = validRange
          } ->
            spendSatisfied inputs signers oiSpendFrom
              P.&& payToSatisfied outputs oiPayTo
              P.&& mustMintSatisfied mintedValue oiMustMint
              P.&& changeSatisfied inputs outputs oiChangeTo
              P.&& feeSatisfied feeValue oiMaxFee
              P.&& intervalSatisfied validRange oiMaxInterval

spendSatisfied :: [PV2.TxInInfo] -> [PV2.PubKeyHash] -> [PV2.Credential] -> Bool
{-# INLINEABLE spendSatisfied #-}
spendSatisfied inputs signers =
  allList (credentialSatisfied inputs signers)

credentialSatisfied :: [PV2.TxInInfo] -> [PV2.PubKeyHash] -> PV2.Credential -> Bool
{-# INLINEABLE credentialSatisfied #-}
credentialSatisfied inputs signers cred =
  credentialSeenInInputs P.|| signerSatisfied
  where
    credentialSeenInInputs = anyList matchesInput inputs
    signerSatisfied = case cred of
      PV2.PubKeyCredential pkh -> anyList (pubKeyHashEquals pkh) signers
      PV2.ScriptCredential _ -> False
    matchesInput txIn = case txIn of
      PV2.TxInInfo _ resolved ->
        case resolved of
          PV2.TxOut addr _ _ _ -> addressHasCredential cred addr

payToSatisfied :: [PV2.TxOut] -> [(PV2.Value, PV2.Address)] -> Bool
payToSatisfied outputs =
  allList (requirementSatisfied outputs)
  where
    requirementSatisfied outs (valueReq, addrReq) =
      anyList (outputMatches valueReq addrReq) outs
    outputMatches valueReq addrReq (PV2.TxOut addr valueOut _ _) =
      addressesEqual addr addrReq P.&& valueGeq valueOut valueReq

mustMintSatisfied :: PV2.Value -> PV2.Value -> Bool
mustMintSatisfied = valueGeq

changeSatisfied :: [PV2.TxInInfo] -> [PV2.TxOut] -> Maybe PV2.Address -> Bool
changeSatisfied inputs outputs =
  maybe P.True checkChange
  where
    checkChange addr =
      let consumed = foldValues inputValue inputs
          produced = foldValues txOutValue outputs
          delta = consumed P.- produced
       in if valuePositive delta
            then anyList (matchesExact addr delta) outputs
            else P.True
    matchesExact addr val (PV2.TxOut outAddr outVal _ _) =
      addressesEqual outAddr addr P.&& valueEq outVal val

feeSatisfied :: PV2.Value -> Maybe Integer -> Bool
feeSatisfied feeValue = maybe P.True (\limit -> adaAmount feeValue P.<= limit)

intervalSatisfied :: PV2.POSIXTimeRange -> Maybe Integer -> Bool
intervalSatisfied range = maybe P.True (maxIntervalOk range)

maxIntervalOk :: PV2.POSIXTimeRange -> Integer -> Bool
maxIntervalOk range limit =
  case range of
    PV2.Interval (PV2.LowerBound lb lc) (PV2.UpperBound ub uc) -> case (lb, ub) of
      (PV2.Finite lo, PV2.Finite hi) ->
        let lo' = adjustLower lc lo
            hi' = adjustUpper uc hi
            width = PV2.getPOSIXTime hi' P.- PV2.getPOSIXTime lo'
         in hi' P.>= lo' P.&& width P.<= limit
      _ -> False
  where
    adjustLower closed time = if closed then time else time P.+ 1
    adjustUpper closed time = if closed then time else time P.- 1

inputValue :: PV2.TxInInfo -> PV2.Value
inputValue (PV2.TxInInfo _ resolved) = txOutValue resolved

txOutValue :: PV2.TxOut -> PV2.Value
txOutValue (PV2.TxOut _ valueOut _ _) = valueOut

foldValues :: (a -> PV2.Value) -> [a] -> PV2.Value
foldValues f = foldrList (\x acc -> f x P.+ acc) P.mempty

valueGeq :: PV2.Value -> PV2.Value -> Bool
valueGeq have need = allList assetSatisfied (Value.flattenValue need)
  where
    assetSatisfied (cs, tn, qtyNeed) =
      case lookupAsset cs tn have of
        Nothing -> P.False
        Just qtyHave -> fromBuiltinBool (BI.lessThanEqualsInteger qtyNeed qtyHave)

lookupAsset :: Value.CurrencySymbol -> Value.TokenName -> PV2.Value -> Maybe Integer
lookupAsset cs tn value =
  go (Value.flattenValue value)
  where
    go [] = Nothing
    go ((cs', tn', amt) : rest)
      | currencySymbolEquals cs cs' P.&& tokenNameEquals tn tn' = Just amt
      | P.otherwise = go rest

valueEq :: PV2.Value -> PV2.Value -> Bool
valueEq a b = valueGeq a b P.&& valueGeq b a

valuePositive :: PV2.Value -> Bool
valuePositive value =
  let flattened = Value.flattenValue value
   in anyList (\(_, _, amt) -> amt P.> 0) flattened

adaAmount :: PV2.Value -> Integer
adaAmount value =
  foldrList step 0 (Value.flattenValue value)
  where
    step (cs, tn, amt) acc =
      let isAda = currencySymbolEquals cs Value.adaSymbol P.&& tokenNameEquals tn Value.adaToken
       in if isAda then acc P.+ amt else acc

allList :: (a -> P.Bool) -> [a] -> P.Bool
allList p = go
  where
    go xs = case xs of
      [] -> P.True
      y : ys -> p y P.&& go ys

anyList :: (a -> P.Bool) -> [a] -> P.Bool
anyList p = go
  where
    go xs = case xs of
      [] -> P.False
      y : ys -> p y P.|| go ys

foldrList :: (a -> b -> b) -> b -> [a] -> b
foldrList f z xs = case xs of
  [] -> z
  y : ys -> f y (foldrList f z ys)

toObserverIntent :: Intent -> Either Text ObserverIntent
toObserverIntent Intent {..} = do
  let mustMint = foldMap LedgerValue.fromCardanoValue irMustMint
      spendFrom = LedgerAddr.cardanoAddressCredential . source <$> irSpendFrom
      payTo =
        fmap
          ( Data.Bifunctor.bimap
              LedgerValue.fromCardanoValue
              LedgerAddr.toPlutusAddress
          )
          irPayTo
      changeTo = LedgerAddr.toPlutusAddress <$> irChangeTo
      maxInterval = fmap ((slotLengthMillis *) . getSlot) irMaxInterval
  pure
    ObserverIntent
      { oiSpendFrom = spendFrom
      , oiPayTo = payTo
      , oiMustMint = mustMint
      , oiChangeTo = changeTo
      , oiMaxFee = irMaxFee
      , oiMaxInterval = maxInterval
      }

-- Helpers that avoid non-inlinable Eq instances on-chain

pubKeyHashEquals :: PV2.PubKeyHash -> PV2.PubKeyHash -> P.Bool
pubKeyHashEquals (PV2.PubKeyHash a) (PV2.PubKeyHash b) = fromBuiltinBool (BI.equalsData (BI.mkB a) (BI.mkB b))

scriptHashEquals :: PV2.ScriptHash -> PV2.ScriptHash -> P.Bool
scriptHashEquals (PV2.ScriptHash a) (PV2.ScriptHash b) = fromBuiltinBool (BI.equalsData (BI.mkB a) (BI.mkB b))

addressesEqual :: PV2.Address -> PV2.Address -> P.Bool
addressesEqual (PV2.Address credL stakeL) (PV2.Address credR stakeR) =
  credentialsEqual credL credR P.&& stakingEquals stakeL stakeR

credentialsEqual :: PV2.Credential -> PV2.Credential -> P.Bool
credentialsEqual (PV2.PubKeyCredential a) (PV2.PubKeyCredential b) = pubKeyHashEquals a b
credentialsEqual (PV2.ScriptCredential a) (PV2.ScriptCredential b) = scriptHashEquals a b
credentialsEqual _ _ = P.False

stakingEquals :: Maybe PV2.StakingCredential -> Maybe PV2.StakingCredential -> P.Bool
stakingEquals Nothing Nothing = P.True
stakingEquals (Just a) (Just b) = stakingCredentialEquals a b
stakingEquals _ _ = P.False

stakingCredentialEquals :: PV2.StakingCredential -> PV2.StakingCredential -> P.Bool
stakingCredentialEquals (PV2.StakingHash a) (PV2.StakingHash b) = credentialsEqual a b
stakingCredentialEquals (PV2.StakingPtr a1 a2 a3) (PV2.StakingPtr b1 b2 b3) =
  fromBuiltinBool (BI.equalsInteger a1 b1)
    P.&& fromBuiltinBool (BI.equalsInteger a2 b2)
    P.&& fromBuiltinBool (BI.equalsInteger a3 b3)
stakingCredentialEquals _ _ = P.False

addressHasCredential :: PV2.Credential -> PV2.Address -> P.Bool
addressHasCredential wanted (PV2.Address cred _) = credentialsEqual wanted cred

currencySymbolEquals :: Value.CurrencySymbol -> Value.CurrencySymbol -> P.Bool
currencySymbolEquals (Value.CurrencySymbol a) (Value.CurrencySymbol b) = fromBuiltinBool (BI.equalsData (BI.mkB a) (BI.mkB b))

tokenNameEquals :: Value.TokenName -> Value.TokenName -> P.Bool
tokenNameEquals (Value.TokenName a) (Value.TokenName b) = fromBuiltinBool (BI.equalsData (BI.mkB a) (BI.mkB b))

fromBuiltinBool :: P.BuiltinBool -> P.Bool
fromBuiltinBool b = BI.ifThenElse b P.True P.False
