{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Treasury where

import           Control.Monad          (void)
import           Data.Text              (Text)
import qualified Data.Map               as Map
import           Plutus.Contract
import           PlutusTx.Prelude       as TxPrelude hiding (Semigroup(..), unless)
--PlutusTx.Numeric
import           Ledger.Ada             as Ada
import           Ledger.Constraints
import qualified Ledger.Tx              as Tx
import           Ledger.Value
import           Prelude                (String, (<>))
--import           Text.Printf            (printf)
import           Plutus.V2.Ledger.Api

--import Params
import Validators
import Setup hiding (iniRegDatum, iniTreDatum, initializeScripts)


-- Only "registered signatories" can open a new payment proposal or vote for
-- approval of an open payment proposal (sign payment).  Funds for payment are
-- taken from the Treasury.  Payment is triggered when the ratio of n' = number
-- of votes to m' = total number of registered signatories is greater than or
-- equal to a predefined ratio n:m (n':m' >= n:m).


-- | Check if a given TxOut carries an NFT with given asset class.
carriesThreadToken :: AssetClass -> Tx.DecoratedTxOut -> Bool
carriesThreadToken a d = assetClassValueOf (Tx._decoratedTxOutValue d) a == 1


-- | Open a new payment proposal from the treasury.
proposePay :: ProposePayParams -> Contract w MultisigSchema Text ()
proposePay ppp = do
  let redeemer  = ProposePay { ppAmount = pppAmount ppp, ppRecipient = pppRecipient ppp, ppDeadline = pppDeadline ppp }
      redeemer' = Redeemer $ toBuiltinData redeemer
      rp        = RegistrarParams (pppRegAC ppp)
      tp        = TreasuryParams (pppTreAC ppp) (pppRegAC ppp)
  utxosR   <- utxosAt $ sigRegAddress rp
  utxosT   <- utxosAt $ treasuryAddress tp
  ownPKH   <- ownFirstPaymentPubKeyHash
  (_, now) <- currentNodeClientTimeRange
  let utxosR' = Map.filter (carriesThreadToken $ pppRegAC ppp) utxosR
      utxosT' = Map.filter (carriesThreadToken $ pppTreAC ppp) utxosT
  case Map.toList <$> [utxosR', utxosT'] of
    [[(orefR, odecR)], [(orefT, odecT)]] -> case snd . Tx._decoratedTxOutScriptDatum <$> [odecR, odecT] of
      [Tx.DatumInline dR, Tx.DatumInline dT] -> case getSigRegDatum dR of
        Just srdatum -> case srState srdatum of
          StandBy -> case getPayDatum dT of
            Nothing       -> logError @String "malformed input treasury datum"
            Just (Just _) -> logError @String "another payment proposal already in progress"
            Just Nothing  -> do
              let treVal  = Tx._decoratedTxOutValue odecT
                  ownVote = [ownPKH]
              if enoughVotes (srRatio srdatum) (srSignatories srdatum) ownVote
                then do
                  let utxos   = Map.union utxosR' utxosT
                      outDat  = Nothing
                      benVal  = lovelaceValueOf (pppAmount ppp)
                      benPKH  = pppRecipient ppp
                      chgVal  = treVal <> (negate benVal)
                      lookups = unspentOutputs utxos                     <>
                                typedValidatorLookups (typedTreasury tp) <>
                                plutusV2OtherScript (treasury tp)
                      tx      = mustSpendScriptOutput orefT redeemer'           <>
                                mustReferenceOutput orefR                       <>
                                mustBeSignedBy ownPKH                           <>
                                mustValidateIn (to now)                         <>
                                mustPayToTheScriptWithInlineDatum outDat chgVal <>
                                mustPayToPubKey  benPKH benVal
                  ledgerTx <- submitTxConstraintsWith @TypedTreasury lookups tx
                  void $ awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx
                  logInfo @String "Tx success: submitted PAYMENT"
                else if null ownVote
                  then logInfo @String "Tx fail: vote was already listed"
                  else do
                    let utxos  = Map.union utxosR' utxosT'
                        outDat = Just $ Paying
                          { payAmount    = pppAmount ppp
                          , payRecipient = pppRecipient ppp
                          , payVotes     = ownVote
                          , payDeadline  = pppDeadline ppp
                          }
                        lookups = unspentOutputs utxos                     <>
                                  typedValidatorLookups (typedTreasury tp) <>
                                  plutusV2OtherScript (treasury tp)
                        tx      = mustSpendScriptOutput orefT redeemer'           <>
                                  mustReferenceOutput orefR                       <>
                                  mustBeSignedBy ownPKH                           <>
                                  mustValidateIn (to now)                         <>
                                  mustPayToTheScriptWithInlineDatum outDat treVal
                    ledgerTx <- submitTxConstraintsWith @TypedTreasury lookups tx
                    void $ awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx
                    logInfo @String "Tx success: created payment proposal"
          _       -> logError @String "can not continue: modification of Registrar's Signatories in progress"
        _            -> logError @String "can not retrieve Registrar's data"
      _                                      -> logError @String "can not retrieve datum"
    _                                    -> logError @String "unable to retrieve data"


-- | Vote for approval of an open payment proposal.
signPay :: SignPayParams -> Contract w MultisigSchema Text ()
signPay spp = do
  let redeemer  = SignPay
      redeemer' = Redeemer $ toBuiltinData redeemer
      rp        = RegistrarParams (sppRegAC spp)
      tp        = TreasuryParams (sppTreAC spp) (sppRegAC spp)
  utxosR   <- utxosAt $ sigRegAddress rp
  utxosT   <- utxosAt $ treasuryAddress tp
  ownPKH   <- ownFirstPaymentPubKeyHash
  (_, now) <- currentNodeClientTimeRange
  let utxosR' = Map.filter (carriesThreadToken $ sppRegAC spp) utxosR
      utxosT' = Map.filter (carriesThreadToken $ sppTreAC spp) utxosT
  case Map.toList <$> [utxosR', utxosT'] of
    [[(orefR, odecR)], [(orefT, odecT)]] -> case snd . Tx._decoratedTxOutScriptDatum <$> [odecR, odecT] of
      [Tx.DatumInline dR, Tx.DatumInline dT] -> case getSigRegDatum dR of
        Just srdatum -> case srState srdatum of
          StandBy -> case getPayDatum dT of
            Nothing            -> logError @String "malformed input treasury datum"
            Just Nothing       -> logError @String "expected a payment proposal"
            Just (Just paying) -> do
              let treVal       = Tx._decoratedTxOutValue odecT
                  currentVotes = nub $ payVotes paying
                  ownVote      = filter (\v -> not . elem v $ currentVotes) [ownPKH]
                  newVotes     = filter (`elem` (srSignatories srdatum)) $ concat [currentVotes, ownVote]
              if enoughVotes (srRatio srdatum) (srSignatories srdatum) newVotes
                then do
                  let utxos   = Map.union utxosR' utxosT
                      outDat  = Nothing
                      benVal  = lovelaceValueOf (payAmount paying)
                      benPKH  = payRecipient paying
                      chgVal  = treVal <> (negate benVal)
                      lookups = unspentOutputs utxos                     <>
                                typedValidatorLookups (typedTreasury tp) <>
                                plutusV2OtherScript (treasury tp)
                      tx      = mustSpendScriptOutput orefT redeemer'           <>
                                mustReferenceOutput orefR                       <>
                                mustBeSignedBy ownPKH                           <>
                                mustValidateIn (to now)                         <>
                                mustPayToTheScriptWithInlineDatum outDat chgVal <>
                                mustPayToPubKey  benPKH benVal
                  ledgerTx <- submitTxConstraintsWith @TypedTreasury lookups tx
                  void $ awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx
                  logInfo @String "Tx success: submitted PAYMENT"
                else if null ownVote
                  then logInfo @String "Tx fail: vote was already listed"
                  else do
                    let utxos  = Map.union utxosR' utxosT'
                        outDat = Just $ Paying
                          { payAmount    = payAmount paying
                          , payRecipient = payRecipient paying
                          , payVotes     = newVotes
                          , payDeadline  = payDeadline paying
                          }
                        lookups = unspentOutputs utxos                     <>
                                  typedValidatorLookups (typedTreasury tp) <>
                                  plutusV2OtherScript (treasury tp)
                        tx      = mustSpendScriptOutput orefT redeemer'           <>
                                  mustReferenceOutput orefR                       <>
                                  mustBeSignedBy ownPKH                           <>
                                  mustValidateIn (to now)                         <>
                                  mustPayToTheScriptWithInlineDatum outDat treVal
                    ledgerTx <- submitTxConstraintsWith @TypedTreasury lookups tx
                    void $ awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx
                    logInfo @String "Tx success: added VOTE to payment proposal"
          _       -> logError @String "can not continue: modification of Registrar's Signatories in progress"
        _            -> logError @String "can not retrieve Registrar's data"
      _                                      -> logError @String "can not retrieve datum"
    _                -> logError @String "unable to retrieve data"
