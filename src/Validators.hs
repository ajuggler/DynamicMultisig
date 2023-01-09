{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Validators where

import           Data.Aeson          (ToJSON, FromJSON)
import           GHC.Generics        (Generic)
import qualified PlutusTx
import           PlutusTx.Prelude       as TxPrelude hiding (unless)
import qualified Plutus.Script.Utils.V2.Scripts  as Scripts
import           Ledger.Ada             as Ada
import qualified Ledger                 as L
import           Ledger.Value
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Contexts as V2LC
import           Plutus.Script.Utils.V2.Typed.Scripts.Validators as V2V

import Params


-- This file defines the script validators for the "Signatories' Registrar" and the "Treasury".


-- +++ Redeemers +++

data SigRegRedeemer = ProposeAdd [L.PaymentPubKeyHash] | ProposeRemove [L.PaymentPubKeyHash] | ApproveAdd | ApproveRemove | Reset

PlutusTx.unstableMakeIsData ''SigRegRedeemer

-- PlutusTx.unstableMakeIsData ''ProposePayment

data PayRedeemer = ProposePay { ppAmount :: Integer, ppRecipient :: L.PaymentPubKeyHash , ppDeadline :: POSIXTime} | SignPay

PlutusTx.unstableMakeIsData ''PayRedeemer

-- +++ Datums +++

data SigRegState =
  StandBy
  | Adding   { addProposed :: [L.PaymentPubKeyHash], addVotes :: [L.PaymentPubKeyHash] }
  | Removing { remProposed :: [L.PaymentPubKeyHash], remVotes :: [L.PaymentPubKeyHash] }

PlutusTx.unstableMakeIsData ''SigRegState

data SigRegDatum = SigRegDatum
  { srSignatories :: [L.PaymentPubKeyHash] -- list of signatories
  , srRatio       :: Ratio                 -- ratio n : m, i.e. n out of m signatories
  , srState       :: SigRegState           -- Registrar's state
  , srDeadline    :: POSIXTime             -- Deadline
  }

PlutusTx.unstableMakeIsData ''SigRegDatum

data Paying = Paying
  { payAmount    :: Integer                -- Payment amount
  , payRecipient :: L.PaymentPubKeyHash    -- Beneficiary
  , payVotes     :: [L.PaymentPubKeyHash]  -- List of signatories that have approved payment,
  , payDeadline  :: POSIXTime              -- Deadline for signing payment
  }

PlutusTx.unstableMakeIsData ''Paying

{-# INLINABLE getPayDatum #-}
getPayDatum :: Datum -> Maybe (Maybe Paying)
getPayDatum = PlutusTx.fromBuiltinData . getDatum

{-# INLINABLE getSigRegDatum #-}
getSigRegDatum :: Datum -> Maybe SigRegDatum
getSigRegDatum = PlutusTx.fromBuiltinData . getDatum

{-# INLINABLE enoughVotes #-}
-- | @enoughVotes (n, m) ts fs == True@  precisely when ratio f:t is greater than or equal to n:m,
-- where  f = length fs  and  t = length ts
enoughVotes :: Ratio -> [L.PaymentPubKeyHash] -> [L.PaymentPubKeyHash] -> Bool
enoughVotes (n, m) totalPKHs fractionPKHs = (length fractionPKHs) * m >= (length totalPKHs) * n


-- SIGNATORIES REGISTRAR --

newtype RegistrarParams = RegistrarParams
  { thTkRegistrar :: AssetClass } deriving (Generic, FromJSON, ToJSON)

PlutusTx.makeLift ''RegistrarParams

-- +++ mkSigRegistrar +++

{-# Inlinable mkSigRegistrar #-}
-- | Validation of operations performed at the Registrar.  The Registrar keeps record of
-- the "list of signatories", which can be dynamically changed, and the threshold ratio
-- n:m, which is static.  The Registrar can be either on a state of "stand-by" or in the
-- process of voting for addition or removal of signatories.
mkSigRegistrar :: RegistrarParams -> SigRegDatum -> SigRegRedeemer -> V2LC.ScriptContext -> Bool
mkSigRegistrar rp dat red ctx =
  traceIfFalse "no thread token found" (valueHasRegThTk $ txOutValue ownInput)              &&
  traceIfFalse "output without thread token" (valueHasRegThTk $ txOutValue ownOutput)       &&
  traceIfFalse "expected continuity of value" (txOutValue ownInput == txOutValue ownOutput) &&
  case red of
    ProposeAdd pkhs    -> traceIfFalse "signatory addition failed" $ validatesProposeAdd pkhs
    ProposeRemove pkhs -> traceIfFalse "signatory removal failed" $ validatesProposeRem pkhs
    ApproveAdd         -> traceIfFalse "approval vote was not validated" validatesApproveAdd
    ApproveRemove      -> traceIfFalse "removal vote was not validated" validatesApproveRem
    Reset              -> traceIfFalse "invalid reset attempt" validatesReset
  where
    validatesProposeAdd :: [L.PaymentPubKeyHash] -> Bool
    validatesProposeAdd ps =
      traceIfFalse "unauthorized signature" authenticated &&
      traceIfFalse "not in 'stand by'" isStateStandBy     &&
      if not . enoughVotes' $ authSignatories
        then traceIfFalse "no new signatory" (not . null $ newSignatories) &&
             case srState outDatum of
               Adding p v -> traceIfFalse "improper filing of new signatories" (sort newSignatories == sort p) &&
                             traceIfFalse "improper filing of initial votes" (sort authSignatories == sort v)
               _          -> traceError "wrong output state"
        else case srState outDatum of
          StandBy -> traceIfFalse "malformed new list of signatories" $
                       validatesNewSignatories $ concat [nub $ srSignatories dat, newSignatories]
          _       -> traceError "wrong output state"
      where
        newSignatories = nub $ filter (\p -> not . elem p $ srSignatories dat) ps

    validatesProposeRem :: [L.PaymentPubKeyHash] -> Bool
    validatesProposeRem ps =
      traceIfFalse "unauthorized signature" authenticated &&
      traceIfFalse "not in 'stand by'" isStateStandBy     &&
      if not . enoughVotes' $ authSignatories
        then traceIfFalse "no signatory to remove" (not . null $ removeSignatories) &&
             case srState outDatum of
               Removing p v -> traceIfFalse "improper filing of signatories to remove" (sort removeSignatories == sort p) &&
                               traceIfFalse "improper filing of initial votes" (sort authSignatories == sort v)
               _            -> traceError "wrong output state"
        else case srState outDatum of
          StandBy -> traceIfFalse "malformed new list of signatories" $
                       validatesNewSignatories $ filter (\p -> not . elem p . nub . srSignatories $ dat) removeSignatories
          _       -> traceError "wrong output state"
      where
        removeSignatories = nub $ filter (`elem` (srSignatories dat)) ps

    validatesApproveAdd :: Bool
    validatesApproveAdd =
      traceIfFalse "unauthorized signature" authenticated &&
      traceIfFalse "deadline has passed" (to (beginTime + srDeadline dat) `L.contains` txInfoValidRange info) &&
      case srState dat of
        Adding ps v ->
          traceIfFalse "no new vote" (not . null $ newVotes) &&
          if not . enoughVotes' $ allVotes
            then case srState outDatum of
                   Adding ps' v' -> traceIfFalse "expected continuity of proposed signatories" (sort ps == sort ps') &&
                                    traceIfFalse "improper filing of new votes" (sort allVotes == sort v')
                   _             -> traceError "worng output state"
            else case srState outDatum of
              StandBy -> traceIfFalse "malformed new list of signatories" $
                           validatesNewSignatories $ concat[nub $ srSignatories dat, ps]
              _       -> traceError "wrong output state"
          where
            newVotes = filter (\p -> not . elem p $ v) authSignatories
            allVotes = concat [v, newVotes]
        _           -> traceError "not in 'Adding' state"

    validatesApproveRem :: Bool
    validatesApproveRem =
      traceIfFalse "unauthorized signature" authenticated &&
      traceIfFalse "deadline has passed" (to (beginTime + srDeadline dat) `L.contains` txInfoValidRange info) &&
      case srState dat of
        Removing ps v ->
          traceIfFalse "no new vote" (not . null $ newVotes) &&
          if not . enoughVotes' $ allVotes
            then case srState outDatum of
                   Removing ps' v' -> traceIfFalse "expected continuity of proposed signatories" (sort ps == sort ps') &&
                                      traceIfFalse "improper filing of new votes" (sort allVotes == sort v')
                   _               -> traceError "worng output state"
            else case srState outDatum of
              StandBy -> traceIfFalse "malformed new list of signatories" $
                           validatesNewSignatories $ filter (\p -> not . elem p $ ps) (nub . srSignatories $ dat)
              _       -> traceError "wrong output state"
          where
            newVotes = filter (\p -> not . elem p $ v) authSignatories
            allVotes = concat [v, newVotes]
        _           -> traceError "not in 'Adding' state"

    validatesReset :: Bool
    validatesReset =
      traceIfFalse "deadline not yet reached" (from (beginTime + srDeadline dat) `L.contains` txInfoValidRange info) &&
      case srState dat of
        Adding {}   -> outStateIsStandBy
        Removing {} -> outStateIsStandBy
        _           -> traceError "already in stand by"
      where
        outStateIsStandBy = case srState outDatum of
          StandBy -> True
          _       -> traceError "wrong output state"

    valueHasRegThTk :: Value -> Bool
    valueHasRegThTk v = assetClassValueOf v (thTkRegistrar rp) == 1

    ownInput :: TxOut
    ownInput = case findOwnInput ctx of
      Nothing -> traceError "registrar's validator has no input"
      Just i  -> txInInfoResolved i
      
    info :: TxInfo
    info = scriptContextTxInfo ctx

    txSignatories :: [L.PaymentPubKeyHash]
    txSignatories = L.PaymentPubKeyHash <$> txInfoSignatories info

    authSignatories :: [L.PaymentPubKeyHash]
    authSignatories = nub $ filter (`elem` (srSignatories dat)) txSignatories

    authenticated :: Bool
    authenticated = not . null $ authSignatories

    enoughVotes' :: [L.PaymentPubKeyHash] -> Bool
    enoughVotes' = enoughVotes (srRatio dat) (srSignatories dat)

    isStateStandBy :: Bool
    isStateStandBy = case srState dat of
      StandBy -> True
      _       -> from (beginTime + srDeadline dat) `L.overlaps` txInfoValidRange info  -- deadline has passed

    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
      [o] -> o
      _   -> traceError "expected exaclty one output"

    outDatum :: SigRegDatum
    outDatum = case txOutDatum ownOutput of
      OutputDatum d -> case getSigRegDatum d of
        Nothing -> traceError "ill defined output datum"
        Just d' -> d'
      _             -> traceError "no output datum"

    validatesNewSignatories :: [L.PaymentPubKeyHash] -> Bool
    validatesNewSignatories ns = sort ns == (sort $ srSignatories outDatum)      

data TypedReg
instance V2V.ValidatorTypes TypedReg where
  type instance DatumType TypedReg    = SigRegDatum
  type instance RedeemerType TypedReg = SigRegRedeemer

typedSigRegistrar :: RegistrarParams -> V2V.TypedValidator TypedReg
typedSigRegistrar rp = V2V.mkTypedValidator @TypedReg
  ($$(PlutusTx.compile [|| mkSigRegistrar ||])
     `PlutusTx.applyCode` PlutusTx.liftCode rp)
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = V2V.mkUntypedValidator @SigRegDatum @SigRegRedeemer

sigRegistrar :: RegistrarParams -> Scripts.Validator
sigRegistrar = V2V.validatorScript . typedSigRegistrar

sigRegHash :: RegistrarParams -> ValidatorHash
sigRegHash = Scripts.validatorHash . sigRegistrar

sigRegAddress :: RegistrarParams -> Address
sigRegAddress = L.scriptHashAddress . sigRegHash


-- TREASURY --

data TreasuryParams = TreasuryParams
  { thTkTreasury :: AssetClass
  , thTkReg      :: AssetClass
  } deriving (Generic, FromJSON, ToJSON)

PlutusTx.makeLift ''TreasuryParams

-- +++ mkTreasury +++

{-# INLINABLE mkTreasury #-}
-- | Validation of operations performed at the Treasury. Datum can be either @Nothing@
-- (the Treasury is on "stand-by" waiting for a payment proposal) or @Just payment@, where
-- @payment@ contains the proposed beneficiary, amount and deadline for approval, as well
-- as keeps record of the votes accumulated so far for approval of the payment proposal.
mkTreasury :: TreasuryParams -> Maybe Paying -> PayRedeemer -> V2LC.ScriptContext -> Bool
mkTreasury tp dat red ctx =
  traceIfFalse "not authenticated" authenticated               &&
  traceIfFalse "no treasury thread token found" hasThreadToken &&
  case red of
    ProposePay {} ->
      traceIfFalse "not initialized" (isNothing dat)  &&
      if not . enoughSignatures $ authTxSignatories
        then traceIfFalse "impromoer datum initialization" properDatumIni
        else traceIfFalse "malformed payment" $ payingToBeneficiary (ppAmount red) (ppRecipient red)
    SignPay       ->
      case dat of
        Just paying -> traceIfFalse "deadline has passed" (to (beginTime + payDeadline paying) `L.contains` txInfoValidRange info) &&
                       if not . enoughSignatures $ validVotes
                         then traceIfFalse "malformed output datum" $ properDatumProgress paying
                         else traceIfFalse "malformed payment" $ payingToBeneficiary (payAmount paying) (payRecipient paying)
        _           -> traceError "no payment proposal to sign"
        
  where
    valueHasTreasuryThTk :: Value -> Bool
    valueHasTreasuryThTk v = assetClassValueOf v (thTkTreasury tp) == 1

    info :: TxInfo
    info = scriptContextTxInfo ctx

    treasuryInput :: TxOut
    treasuryInput = case findOwnInput ctx of
      Nothing -> traceError "votes input missing"
      Just i  -> txInInfoResolved i

    txSignatories :: [L.PaymentPubKeyHash]
    txSignatories = L.PaymentPubKeyHash <$> txInfoSignatories info

    hasThreadToken :: Bool
    hasThreadToken = valueHasTreasuryThTk . txOutValue $ treasuryInput

    carriesThreadToken :: AssetClass -> TxOut -> Bool
    carriesThreadToken a i = assetClassValueOf (txOutValue i) a == 1

    registrarData :: Maybe ([L.PaymentPubKeyHash], Ratio)
    registrarData =
      let
        refs = filter (carriesThreadToken $ thTkReg tp) (txInInfoResolved <$> txInfoReferenceInputs info)
      in
        case txOutDatum <$> refs of
          [OutputDatum d] -> (\srdat -> (srSignatories srdat, srRatio srdat)) <$> (getSigRegDatum d)
          _               -> Nothing

    regSignatories :: Maybe [L.PaymentPubKeyHash]
    regSignatories = fst <$> registrarData

    approveRatio :: Maybe Ratio
    approveRatio = snd <$> registrarData

    filterSignatories :: [L.PaymentPubKeyHash] -> [L.PaymentPubKeyHash]
    filterSignatories pkhs = case regSignatories of
      Nothing -> []
      Just sgs -> filter (`elem` sgs) pkhs

    authTxSignatories :: [L.PaymentPubKeyHash]
    authTxSignatories = filterSignatories txSignatories

    authenticated :: Bool
    authenticated = not . null $ authTxSignatories
    
    ownOutputs :: [TxOut]
    ownOutputs = getContinuingOutputs ctx

    validVotes :: [L.PaymentPubKeyHash]
    validVotes = nub . concat $ [filterSignatories previousVotes, authTxSignatories]
      where
        previousVotes = case dat of
          Nothing     -> []
          Just paying -> payVotes paying

    properDatumIni :: Bool
    properDatumIni = case txOutDatum <$> ownOutputs of
      [OutputDatum odat] -> case getPayDatum odat of
        Just (Just paying) ->
          traceIfFalse "wrong output amount" (ppAmount red == payAmount paying)                                &&
          traceIfFalse "amount is too small" (payAmount paying > valueOf valMinAda Ada.adaSymbol Ada.adaToken) &&
          traceIfFalse "wrong output recipient" (ppRecipient red == payRecipient paying)                       &&
          traceIfFalse "invalid initial vote(s)" (sort (payVotes paying) == sort authTxSignatories)
        _                  -> traceError "malformed output datum"
      _                  -> traceError "no output datum"
        
    properDatumProgress :: Paying -> Bool
    properDatumProgress paying = case ownOutputs of
      [o] -> case txOutDatum o of
        OutputDatum odat -> case getPayDatum odat of
          Just (Just paying') -> valueHasTreasuryThTk (txOutValue o)              &&
                                            payAmount paying'         == payAmount paying    &&
                                            payRecipient paying'      == payRecipient paying &&
                                            (sort $ payVotes paying') == sort validVotes
          _                              -> traceError "malformed output datum"
        _                -> traceError "no output datum"
      _   -> traceError "expected unique output"

    enoughSignatures :: [L.PaymentPubKeyHash] -> Bool
    enoughSignatures = case (regSignatories, approveRatio) of
      (Just sgs, Just art) -> enoughVotes art sgs
      _                    -> traceError "corrupted reference data"

    payingToBeneficiary :: Integer -> L.PaymentPubKeyHash -> Bool
    payingToBeneficiary amt pkh =
      let
        refs = filter (carriesThreadToken $ thTkTreasury tp) ownOutputs
      in case refs of
           [o] -> let valueCont  = mconcat (txOutValue <$> ownOutputs)
                      valuePaid  = valuePaidTo info . L.unPaymentPubKeyHash $ pkh
                      valueToPay = lovelaceValueOf amt
                  in  traceIfFalse "paying wrong amount" (valuePaid == valueToPay)                                  &&
                      traceIfFalse "unauthorized spending" ((txOutValue treasuryInput) == (valuePaid <> valueCont)) &&
                      case txOutDatum o of
                        NoOutputDatum    -> traceIfTrue "warning: no output datum" True
                        OutputDatum odat -> case getPayDatum odat of
                          Just Nothing -> True
                          _                       -> traceError "output datum messed up" 
                        _                -> traceError "output datum messed up"
           _   -> traceError "lost thread token"

data TypedTreasury
instance V2V.ValidatorTypes TypedTreasury where
  type instance DatumType TypedTreasury    = Maybe Paying
  type instance RedeemerType TypedTreasury = PayRedeemer

typedTreasury :: TreasuryParams -> V2V.TypedValidator TypedTreasury
typedTreasury tp = V2V.mkTypedValidator @TypedTreasury
  ($$(PlutusTx.compile [|| mkTreasury ||])
     `PlutusTx.applyCode` PlutusTx.liftCode tp)
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = V2V.mkUntypedValidator @(Maybe Paying) @PayRedeemer

treasury :: TreasuryParams -> Validator
treasury = V2V.validatorScript . typedTreasury

treasuryHash :: TreasuryParams -> ValidatorHash
treasuryHash = Scripts.validatorHash . treasury

treasuryAddress :: TreasuryParams -> Address
treasuryAddress = L.scriptHashAddress . treasuryHash

