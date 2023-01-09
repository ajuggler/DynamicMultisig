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

module Registrar where

import           Control.Monad          (void)
import           Data.Text              (Text)
import qualified Data.Map               as Map
import           Plutus.Contract
import           PlutusTx.Prelude       as TxPrelude hiding (Semigroup(..), unless)
import           Ledger.Constraints
import qualified Ledger.Tx              as Tx
import           Ledger.Value
import           Prelude                (Show(..), String, (<>))
import           Text.Printf            (printf)
import           Plutus.V2.Ledger.Api

--import Params
import Validators
import Setup hiding (iniRegDatum, iniTreDatum, initializeScripts)


-- The Registrar keeps track of the list or "registered signatories" and a predefined
-- ratio n:m.  Proposals for payment from the Treasury look at these two pieces of data
-- for approval: only registered signatories can open proposals or vote in favor of an
-- open payment proposal, and the payment is triggered when the ratio of votes to total
-- signatories equals or is above than the ratio n:m.

-- Through a voting mechanism, new signatories can be added and previously registered
-- signatories can be removed.


-- | Propose registration of new signatory.
proposeAddSig :: PropAddSigParams -> Contract w MultisigSchema Text ()
proposeAddSig pasp = do
  let redeemer       = ProposeAdd $ paspAddSignatories pasp
      addSignatories = paspAddSignatories pasp
  utxos  <- utxosAt . sigRegAddress . RegistrarParams . paspRegAC $ pasp
  ownPKH <- ownFirstPaymentPubKeyHash
  case Map.toList $ Map.filter (\d -> assetClassValueOf (Tx._decoratedTxOutValue d) (paspRegAC pasp) == 1) utxos of
    [(oref, odec)] -> case snd $ Tx._decoratedTxOutScriptDatum odec of
      Tx.DatumInline d -> case getSigRegDatum d of
        Nothing -> logInfo @String "malformed datum"
        Just inputDat -> case srState inputDat of
          StandBy -> do
            let currentSignatories = srSignatories inputDat
                newVotes           = filter (`elem` currentSignatories) [ownPKH]  -- only current Signatories can vote
                redeemer'          = Redeemer $ toBuiltinData redeemer
                outputVal          = Tx._decoratedTxOutValue odec
            if enoughVotes (srRatio inputDat) currentSignatories newVotes
              then do
                logInfo @String "enough votes to modify Signatories' list"
                let newSignatories = concat[currentSignatories, addSignatories]
                    outputDat      = SigRegDatum
                      { srSignatories = newSignatories
                      , srRatio       = srRatio inputDat
                      , srState       = StandBy
                      , srDeadline    = POSIXTime 0
                      }
                    lookups = unspentOutputs utxos                                                           <>
                              typedValidatorLookups (typedSigRegistrar . RegistrarParams . paspRegAC $ pasp) <>
                              plutusV2OtherScript (sigRegistrar . RegistrarParams . paspRegAC $ pasp)
                    tx      = mustSpendScriptOutput oref redeemer'                  <>
                              mustBeSignedBy ownPKH                                 <>
                              mustPayToTheScriptWithInlineDatum outputDat outputVal
                ledgerTx <- submitTxConstraintsWith @TypedReg lookups tx
                void $ awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx
                logInfo @String $ printf "Tx success: added %d new signatory(ies)" (length addSignatories)
              else do
                logInfo @String "not enough votes to modify Signatories' list yet"
                let outputDat = SigRegDatum
                      { srSignatories = srSignatories inputDat
                      , srRatio = srRatio inputDat
                      , srState = Adding
                          { addProposed = paspAddSignatories pasp
                          , addVotes    = newVotes
                          }
                      , srDeadline = paspDeadline pasp
                      }
                    lookups = unspentOutputs utxos                                                           <>
                              typedValidatorLookups (typedSigRegistrar . RegistrarParams . paspRegAC $ pasp) <>
                              plutusV2OtherScript (sigRegistrar . RegistrarParams . paspRegAC $ pasp)
                    tx      = mustSpendScriptOutput oref redeemer'                  <>
                              mustBeSignedBy ownPKH                                 <>
                              mustPayToTheScriptWithInlineDatum outputDat outputVal
                ledgerTx <- submitTxConstraintsWith @TypedReg lookups tx
                void $ awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx
                logInfo @String $ printf "Tx success: registered %d proposed signatory(ies)" (length $ paspAddSignatories pasp)
          _       -> logInfo @String "expected Registrar in stand by"
      _                -> logInfo @String "datum not found"
    _              -> logInfo @String "thread token not found"


-- | Vote in favor of proposed addition of new signatory.
approveAddSig :: ApproveAddParams -> Contract w MultisigSchema Text ()
approveAddSig aap = do
  let redeemer       = ApproveAdd
  utxos  <- utxosAt . sigRegAddress . RegistrarParams . aapRegAC $ aap
  ownPKH <- ownFirstPaymentPubKeyHash
  case Map.toList $ Map.filter (\d -> assetClassValueOf (Tx._decoratedTxOutValue d) (aapRegAC aap) == 1) utxos of
    [(oref, odec)] -> case snd $ Tx._decoratedTxOutScriptDatum odec of
      Tx.DatumInline d -> case getSigRegDatum d of
        Nothing -> logInfo @String "malformed datum"
        Just inputDat -> case srState inputDat of
          Adding ps vs -> do
            (_, now) <- currentNodeClientTimeRange
            -- logInfo @String $ printf "current time: %s" (show now)
            let currentSignatories = srSignatories inputDat
                addingVotes        = filter (`elem` currentSignatories) [ownPKH]  -- only current Signatories can vote
                newVotes           = concat[vs, addingVotes]
                redeemer'          = Redeemer $ toBuiltinData redeemer
                outputVal          = Tx._decoratedTxOutValue odec
            if enoughVotes (srRatio inputDat) currentSignatories newVotes
              then do
                logInfo @String "enough votes to modify Signatories' list"
                let newSignatories = concat[currentSignatories, ps]
                    outputDat      = SigRegDatum
                      { srSignatories = newSignatories
                      , srRatio       = srRatio inputDat
                      , srState       = StandBy
                      , srDeadline    = POSIXTime 0
                      }
                    lookups = unspentOutputs utxos                                                           <>
                              typedValidatorLookups (typedSigRegistrar . RegistrarParams . aapRegAC $ aap) <>
                              plutusV2OtherScript (sigRegistrar . RegistrarParams . aapRegAC $ aap)
                    tx      = mustSpendScriptOutput oref redeemer'                  <>
                              mustBeSignedBy ownPKH                                 <>
                              mustValidateIn (to now)                               <>
                              mustPayToTheScriptWithInlineDatum outputDat outputVal
                ledgerTx <- submitTxConstraintsWith @TypedReg lookups tx
                void $ awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx
                logInfo @String $ printf "Tx success: added %d new signatory(ies)" (length ps)
              else do
                logInfo @String "not enough votes to modify Signatories' list yet"
                let outputDat = SigRegDatum
                      { srSignatories = srSignatories inputDat
                      , srRatio       = srRatio inputDat
                      , srState       = Adding
                          { addProposed = ps
                          , addVotes    = newVotes
                          }
                      , srDeadline = srDeadline inputDat
                      }
                    lookups = unspentOutputs utxos                                                           <>
                              typedValidatorLookups (typedSigRegistrar . RegistrarParams . aapRegAC $ aap) <>
                              plutusV2OtherScript (sigRegistrar . RegistrarParams . aapRegAC $ aap)
                    tx      = mustSpendScriptOutput oref redeemer'                  <>
                              mustBeSignedBy ownPKH                                 <>
                              mustValidateIn (to now)                               <>
                              mustPayToTheScriptWithInlineDatum outputDat outputVal
                ledgerTx <- submitTxConstraintsWith @TypedReg lookups tx
                void $ awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx
                logInfo @String $ printf "Tx success: added %d new vote(s)" (length $ addingVotes)
          _       -> logInfo @String "expected Registrar in 'Adding' state"
      _                -> logInfo @String "datum not found"
    _              -> logInfo @String "thread token not found"


-- | Report registrar's state (current signatories and, if applicable, current votes.
reportRegState :: ReportParams -> Contract w MultisigSchema Text ()
reportRegState rp = do
  utxos <- utxosAt . sigRegAddress. RegistrarParams . rpRegAC $ rp
  case Map.toList $ Map.filter (\d -> assetClassValueOf (Tx._decoratedTxOutValue d) (rpRegAC rp) == 1) utxos of
    [(_, odec)] -> case snd $ Tx._decoratedTxOutScriptDatum odec of
      Tx.DatumInline d -> case getSigRegDatum d of
        Nothing       -> logInfo @String "malformed datum"
        Just inputDat -> do
          logInfo @String . printf "Current Signatories: %s" . show . srSignatories $ inputDat
          case srState inputDat of
            StandBy          -> return ()
            Adding _ _   -> do
              logInfo @String . printf "Proposed Signatories for addition: %s" . show . addProposed . srState $ inputDat
              logInfo @String . printf "Current votes for addition: %s" . show . addVotes . srState $ inputDat
            Removing _ _ -> do
              logInfo @String . printf "Proposed Signatories for deletion: %s" . show . remProposed . srState $ inputDat
              logInfo @String . printf "Current votes for deletion: %s" . show . remVotes . srState $ inputDat
      _                -> logInfo @String "datum2 not found"
    _           -> logInfo @String "no thread token found"

