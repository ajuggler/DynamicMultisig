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

module Setup where

import           Control.Monad          (void)
import           Data.Aeson          (ToJSON, FromJSON)
import           Data.Text              (Text)
import           Data.Void              (Void)
import qualified Data.Map               as Map
import           GHC.Generics        (Generic)
import           Plutus.Contract
import           PlutusTx.Prelude       as TxPrelude hiding (Semigroup(..), unless)
import           Ledger.Constraints
import qualified Ledger                 as L
import qualified Ledger.Tx              as Tx
import           Ledger.Value
import           Prelude                (String, (<>))
--import           Text.Printf            (printf)
import           Plutus.V2.Ledger.Api

import Params
import Validators


-- SCHEMA --

type MultisigSchema = Endpoint "initializeScripts" InitializeParams
                  .\/ Endpoint "proposeAddSig" PropAddSigParams
                  .\/ Endpoint "approveAddSig" ApproveAddParams
                  .\/ Endpoint "reportRegState" ReportParams
                  .\/ Endpoint "signPay" SignPayParams
                  .\/ Endpoint "proposePay" ProposePayParams

data InitializeParams = InitializeParams
  { ipRegAC       :: !AssetClass                   -- Registrar's thread token as asset
  , ipTreAC       :: !AssetClass                   -- Treasury's thread token as asset
  , ipSignatories :: ![L.PaymentPubKeyHash]        -- initial Signatories
  , ipRatio       :: !Ratio                        -- Ratio n:m (n out of m)
  , ipDeadlineR   :: !POSIXTime                    -- initial deadline
  , ipAmount      :: !Integer                      -- Payment amount
  , ipBeneficiary :: !(Maybe L.PaymentPubKeyHash)  -- Maybe a beneficiary (if payment proposal exists)
  , ipVotes       :: ![L.PaymentPubKeyHash]        -- Votes for payment release
  , ipDeadlineT   :: !POSIXTime                    -- initial deadline
  } deriving (Generic, FromJSON, ToJSON)

data PropAddSigParams = PropAddSigParams
  { paspAddSignatories :: ![L.PaymentPubKeyHash] -- propose add Signatories
  , paspRegAC          :: !AssetClass            -- Registrar's thread token as asset
  , paspDeadline       :: !POSIXTime             -- deadline for Signatorie's approval
  } deriving (Generic, FromJSON, ToJSON)

newtype ApproveAddParams = ApproveAddParams
  { aapRegAC :: AssetClass  -- Registrar's thread token as asset
  } deriving (Generic, FromJSON, ToJSON)

data ReportParams = ReportParams
  { rpRegAC :: !AssetClass  -- Registrar's thread token as asset
  } deriving (Generic, FromJSON, ToJSON)

data ProposePayParams = ProposePayParams
  { pppRegAC     :: !AssetClass           -- Registrar's thread token as asset
  , pppTreAC     :: !AssetClass           -- Treasury's thread token as asset
  , pppAmount    :: !Integer              -- Amount to pay
  , pppRecipient :: !L.PaymentPubKeyHash  -- Beneficiary
  , pppDeadline  :: !POSIXTime            -- deadline for payment approval
  } deriving (Generic, FromJSON, ToJSON)

data SignPayParams = SignPayParams
  { sppRegAC :: !AssetClass  -- Registrar's thread token as asset
  , sppTreAC :: !AssetClass  -- Treasury's thread token as asset
  } deriving (Generic, FromJSON, ToJSON)


-- SETUP REGISTRAR & TREASURY --

-- Initialization of the Registrar's and Treasury's states, to be performed by the
-- "Founder", who in particular chooses the initial list of Signatories and the "n out of
-- m" threshold ratio.

-- | Initial datum at Registrar
iniRegDatum :: [L.PaymentPubKeyHash] -> Ratio -> POSIXTime -> SigRegDatum
iniRegDatum ps r d = SigRegDatum
  { srSignatories = ps
  , srRatio       = r
  , srState       = StandBy
  , srDeadline    = d
  }

-- | Initial datum at Treasury
iniTreDatum :: Integer -> Maybe L.PaymentPubKeyHash -> [L.PaymentPubKeyHash] -> POSIXTime -> Maybe Paying
iniTreDatum a mb vs d = case mb of
  Nothing -> Nothing
  Just b  -> Just $ Paying
          { payAmount    = a
          , payRecipient = b
          , payVotes     = vs
          , payDeadline  = d
          }

-- | Initializes states of both the Registrar and the Treasury
initializeScripts :: InitializeParams -> Contract w MultisigSchema Text ()
initializeScripts ip = do
  utxos <- ownUtxos
  let oref    = head $ Map.keys utxos
      regScriptHash = sigRegHash . RegistrarParams . ipRegAC $ ip
      treScriptHash = treasuryHash $ TreasuryParams (ipTreAC ip) (ipRegAC ip)
      regDat  = Datum . toBuiltinData $ iniRegDatum (ipSignatories ip) (ipRatio ip) (ipDeadlineR ip)
      treDat  = Datum . toBuiltinData $ iniTreDatum (ipAmount ip) (ipBeneficiary ip) (ipVotes ip) (ipDeadlineT ip)
      regVal  = valMinAda <> assetClassValue (ipRegAC ip) 1
      treVal  = treasuryFunding <> assetClassValue (ipTreAC ip) 1
      lookups = unspentOutputs utxos
      tx      = mustSpendPubKeyOutput oref                                      <>
                mustPayToOtherScriptWithInlineDatum regScriptHash regDat regVal <>
                mustPayToOtherScriptWithInlineDatum treScriptHash treDat treVal
  ledgerTx <- submitTxConstraintsWith @Void lookups tx
  void $ awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx
  logInfo @String "initialized Treasury & Registrar"

