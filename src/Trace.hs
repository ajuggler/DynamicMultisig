{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications   #-}

module Trace where

import Control.Monad          (void)
import Data.Default           (Default (..))
import qualified Data.Map                   as Map
import           Ledger       (POSIXTime (..), PaymentPubKeyHash)
import           Ledger.Value
import           Ledger.Ada                 as Ada
--import           Ledger.TimeSlot
import           Plutus.Trace.Emulator      as Emulator
import Data.Text              (Text)
import Plutus.Contract        hiding (waitNSlots)
import           PlutusTx.Prelude
import           Prelude                    (IO)
import Wallet.Emulator.Wallet


import Params
--import Validators
import Setup
import Registrar
import Treasury


-- ENDPOINTS --

endpoints :: Contract () MultisigSchema Text ()
endpoints = awaitPromise (initializeScripts' `select` proposeAddSig' `select` approveAddSig'
                          `select` reportRegState' `select` proposePay' `select` signPay') >> endpoints
  where
    initializeScripts' = endpoint @"initializeScripts" initializeScripts
    proposeAddSig'     = endpoint @"proposeAddSig" proposeAddSig
    approveAddSig'     = endpoint @"approveAddSig" approveAddSig
    reportRegState'    = endpoint @"reportRegState" reportRegState
    proposePay'        = endpoint @"proposePay" proposePay
    signPay'           = endpoint @"signPay" signPay


-- TEST --

test :: IO ()
test = runEmulatorTraceIO' def emCfg trace3  -- change this line to run other traces

-- Traces defined below: trace1, trace1', trace1'', trace2, trace2', trace2'', trace2''', trace3


-- Initialization --

wF, wS1, wS2, wS3, wS4, wB :: Wallet
wF  = knownWallet 1 -- Founder
wS1 = knownWallet 2 -- Signatory 1
wS2 = knownWallet 3 -- Signatory 2
wS3 = knownWallet 4 -- Signatory 3
wS4 = knownWallet 5 -- Signatory 4
wB  = knownWallet 6 -- Beneficiary

vF, vS, vB :: Value  -- For funding wallets
vF = Ada.lovelaceValueOf 1_005_000_000  -- Founder
vS = Ada.lovelaceValueOf    10_000_000  -- Signatory
vB = Ada.lovelaceValueOf             0  -- Beneficiary

pkhF, pkhS1, pkhS2, pkhS3, pkhS4, pkhB :: PaymentPubKeyHash
pkhF  = mockWalletPaymentPubKeyHash wF
pkhS1 = mockWalletPaymentPubKeyHash wS1
pkhS2 = mockWalletPaymentPubKeyHash wS2
pkhS3 = mockWalletPaymentPubKeyHash wS3
pkhS4 = mockWalletPaymentPubKeyHash wS4
pkhB  = mockWalletPaymentPubKeyHash wB

emCfg :: EmulatorConfig
emCfg = EmulatorConfig
  { _initialChainState = Left $ Map.fromList
       [ (wF, vF <> (singleton thTkRegistrarCS thTkRegistrarNm 1) <> (singleton thTkTreasuryCS thTkTreasuryNm 1))
       , (wS1, vS), (wS2, vS), (wS3, vS), (wS4, vS)
       , (wB, vB)
       ]
  , _params = def
  }

deadline0, deadline1, deadline2 :: POSIXTime
deadline0 = POSIXTime 0
deadline1 = POSIXTime 5000
deadline2 = POSIXTime 10000
-- Observed that Emulator has trouble managing times larger than 'POSIXTime 1000000000'


-- EMULATOR TRACE --

-- +++ trace1 +++

-- Summary: Triggering Payment with one vote.

trace1 :: EmulatorTrace ()
trace1 = do
  hF  <- activateContractWallet wF endpoints
--  hS1 <- activateContractWallet wS1 endpoints
  hS2 <- activateContractWallet wS2 endpoints
--  hS3 <- activateContractWallet wS3 endpoints

  -- | Initialization with three registered signatories and payment proposal
  -- activated.
  callEndpoint @"initializeScripts" hF $ InitializeParams
    { ipRegAC       = assetClassRegistrar
    , ipTreAC       = assetClassTreasury
    , ipSignatories = [pkhS1, pkhS2, pkhS3]
    , ipRatio       = (2, 3)
    , ipDeadlineR   = deadline0
    , ipAmount      = 55_000_000
    , ipBeneficiary = Just pkhB
    , ipVotes       = [pkhS1]
    , ipDeadlineT   = deadline2
    }
 
  void $ Emulator.waitNSlots 1

  -- | Sign payment (completes 2 out of 3)
  callEndpoint @"signPay" hS2 $ SignPayParams
    { sppRegAC = assetClassRegistrar
    , sppTreAC = assetClassTreasury
    }

  void $ Emulator.waitNSlots 1

-- +++ trace1' +++

-- Summary: Payment proposal pending, one vote added succesffully, second vote.
-- arrives too late.

trace1' :: EmulatorTrace ()
trace1' = do
  hF  <- activateContractWallet wF endpoints
  hS1 <- activateContractWallet wS1 endpoints
  hS2 <- activateContractWallet wS2 endpoints
--  hS3 <- activateContractWallet wS3 endpoints

  -- | Initialization with three signatories registered, payment proposal active,
  -- no one has signed payment.
  callEndpoint @"initializeScripts" hF $ InitializeParams
    { ipRegAC       = assetClassRegistrar
    , ipTreAC       = assetClassTreasury
    , ipSignatories = [pkhS1, pkhS2, pkhS3]
    , ipRatio       = (2, 3)
    , ipDeadlineR   = deadline0
    , ipAmount      = 55_000_000
    , ipBeneficiary = Just pkhB
    , ipVotes       = []
    , ipDeadlineT   = deadline1
    }
 
  void $ Emulator.waitNSlots 1

  -- | Signs payment (first of 2 out of 3)
  callEndpoint @"signPay" hS1 $ SignPayParams
    { sppRegAC = assetClassRegistrar
    , sppTreAC = assetClassTreasury
    }

  void $ Emulator.waitNSlots 5

  -- | Signs payment (second of 2 out of 3) but it's too late
  callEndpoint @"signPay" hS2 $ SignPayParams
    { sppRegAC = assetClassRegistrar
    , sppTreAC = assetClassTreasury
    }

  void $ Emulator.waitNSlots 1

-- +++ trace1'' +++

-- Summary: Malicious actor tries to trick the validator trying to count a vote
-- twice; later and before the deadline the Payment is triggered by a legitimate
-- second vote.

trace1'' :: EmulatorTrace ()
trace1'' = do
  hF  <- activateContractWallet wF endpoints
  hS1 <- activateContractWallet wS1 endpoints
  hS2 <- activateContractWallet wS2 endpoints
--  hS3 <- activateContractWallet wS3 endpoints

  -- | Initializes with 3 signatories registered, payment proposal active.
  callEndpoint @"initializeScripts" hF $ InitializeParams
    { ipRegAC       = assetClassRegistrar
    , ipTreAC       = assetClassTreasury
    , ipSignatories = [pkhS1, pkhS2, pkhS3]
    , ipRatio       = (2, 3)
    , ipDeadlineR   = deadline0
    , ipAmount      = 55_000_000
    , ipBeneficiary = Just pkhB
    , ipVotes       = []
    , ipDeadlineT   = deadline2
    }
 
  void $ Emulator.waitNSlots 1

  -- | Signs payment (first of 2 out of 3)
  callEndpoint @"signPay" hS1 $ SignPayParams
    { sppRegAC = assetClassRegistrar
    , sppTreAC = assetClassTreasury
    }

  void $ Emulator.waitNSlots 1

  -- | Same signatory tries to sign again
  callEndpoint @"signPay" hS1 $ SignPayParams
    { sppRegAC = assetClassRegistrar
    , sppTreAC = assetClassTreasury
    }

  void $ Emulator.waitNSlots 1

  -- | Another signatory signs payment (second of 2 out of 3)
  callEndpoint @"signPay" hS2 $ SignPayParams
    { sppRegAC = assetClassRegistrar
    , sppTreAC = assetClassTreasury
    }

  void $ Emulator.waitNSlots 1

-- +++ trace2 +++

-- Summary: Payment proposal triggers "payment" since only one vote is needed.

trace2 :: EmulatorTrace ()
trace2 = do
  hF  <- activateContractWallet wF endpoints
  hS1 <- activateContractWallet wS1 endpoints
--  hS2 <- activateContractWallet wS2 endpoints
--  hS3 <- activateContractWallet wS3 endpoints

  -- | Initializes with one signatory registered, treasury on stand-by.
  callEndpoint @"initializeScripts" hF $ InitializeParams
    { ipRegAC       = assetClassRegistrar
    , ipTreAC       = assetClassTreasury
    , ipSignatories = [pkhS1]
    , ipRatio       = (2, 3)
    , ipDeadlineR   = deadline0
    , ipAmount      = 0
    , ipBeneficiary = Nothing
    , ipVotes       = []
    , ipDeadlineT   = deadline0
    }
 
  void $ Emulator.waitNSlots 1

  -- | Signatory opens a payment proposal, triggering payment (only one vote needed)
  callEndpoint @"proposePay" hS1 $ ProposePayParams
    { pppRegAC = assetClassRegistrar
    , pppTreAC = assetClassTreasury
    , pppAmount = 55_000_000
    , pppRecipient = pkhB
    , pppDeadline = deadline2
    }

  void $ Emulator.waitNSlots 1

-- +++ trace2' +++

-- Summary: Payment proposal is submitted. A second payment proposal is rejected
-- sine the first one is in progress.

trace2' :: EmulatorTrace ()
trace2' = do
  hF  <- activateContractWallet wF endpoints
  hS1 <- activateContractWallet wS1 endpoints
  hS2 <- activateContractWallet wS2 endpoints
--  hS3 <- activateContractWallet wS3 endpoints

  -- | Initializes with two signatories registered, treasury on stand-by.
  callEndpoint @"initializeScripts" hF $ InitializeParams
    { ipRegAC       = assetClassRegistrar
    , ipTreAC       = assetClassTreasury
    , ipSignatories = [pkhS1, pkhS2]
    , ipRatio       = (2, 3)
    , ipDeadlineR   = deadline0
    , ipAmount      = 0
    , ipBeneficiary = Nothing
    , ipVotes       = []
    , ipDeadlineT   = deadline0
    }
 
  void $ Emulator.waitNSlots 1

  -- | Signatory opens payment proposal
  callEndpoint @"proposePay" hS1 $ ProposePayParams
    { pppRegAC     = assetClassRegistrar
    , pppTreAC     = assetClassTreasury
    , pppAmount    = 55_000_000
    , pppRecipient = pkhB
    , pppDeadline  = deadline2
    }

  void $ Emulator.waitNSlots 1

  -- | Signatory tries to open a second payment proposal (should fail)
  callEndpoint @"proposePay" hS2 $ ProposePayParams
    { pppRegAC = assetClassRegistrar
    , pppTreAC = assetClassTreasury
    , pppAmount = 75_000_000
    , pppRecipient = pkhB
    , pppDeadline = deadline2
    }

  void $ Emulator.waitNSlots 1

-- +++ trace2'' +++

-- Summary: Malicious signatory submits a payment proposal and tries to make his
-- vote count twice, which fails.

trace2'' :: EmulatorTrace ()
trace2'' = do
  hF  <- activateContractWallet wF endpoints
  hS1 <- activateContractWallet wS1 endpoints
--  hS2 <- activateContractWallet wS2 endpoints
--  hS3 <- activateContractWallet wS3 endpoints

  -- | Initializes with two signatories registered, treasury on stand-by.
  callEndpoint @"initializeScripts" hF $ InitializeParams
    { ipRegAC       = assetClassRegistrar
    , ipTreAC       = assetClassTreasury
    , ipSignatories = [pkhS1, pkhS2]
    , ipRatio       = (2, 3)
    , ipDeadlineR   = deadline0
    , ipAmount      = 0
    , ipBeneficiary = Nothing
    , ipVotes       = []
    , ipDeadlineT   = deadline2
    }
 
  void $ Emulator.waitNSlots 1

  -- | Signatory opens payment proposal
  callEndpoint @"proposePay" hS1 $ ProposePayParams
    { pppRegAC     = assetClassRegistrar
    , pppTreAC     = assetClassTreasury
    , pppAmount    = 55_000_000
    , pppRecipient = pkhB
    , pppDeadline  = deadline2
    }

  void $ Emulator.waitNSlots 1

  -- | Same signatory tries to "double-vote" (should fail)
  callEndpoint @"signPay" hS1 $ SignPayParams
    { sppRegAC = assetClassRegistrar
    , sppTreAC = assetClassTreasury
    }

  void $ Emulator.waitNSlots 1

-- +++ trace2''' +++

-- Summary: Payment proposal is submitted and then receives votes completing th3
-- 3 out of 4 needed to trigger payment.

trace2''' :: EmulatorTrace ()
trace2''' = do
  hF  <- activateContractWallet wF endpoints
  hS1 <- activateContractWallet wS1 endpoints
  hS2 <- activateContractWallet wS2 endpoints
  hS3 <- activateContractWallet wS3 endpoints

  -- | Initializes with 3 signatories registered, treasury on stand-by.
  callEndpoint @"initializeScripts" hF $ InitializeParams
    { ipRegAC       = assetClassRegistrar
    , ipTreAC       = assetClassTreasury
    , ipSignatories = [pkhS1, pkhS2, pkhS3]
    , ipRatio       = (3,4)
    , ipDeadlineR   = deadline0
    , ipAmount      = 0
    , ipBeneficiary = Nothing
    , ipVotes       = []
    , ipDeadlineT   = deadline2
    }
 
  void $ Emulator.waitNSlots 1

  -- | Signatory opens a payment proposal
  callEndpoint @"proposePay" hS1 $ ProposePayParams
    { pppRegAC = assetClassRegistrar
    , pppTreAC = assetClassTreasury
    , pppAmount = 55_000_000
    , pppRecipient = pkhB
    , pppDeadline = deadline2
    }

  void $ Emulator.waitNSlots 1

  -- | Another signatory signs payment (not yet enough votes)
  callEndpoint @"signPay" hS2 $ SignPayParams
    { sppRegAC = assetClassRegistrar
    , sppTreAC = assetClassTreasury
    }

  void $ Emulator.waitNSlots 1

  -- | Yet another signatory signs payment, resulting in enough votes
  callEndpoint @"signPay" hS3 $ SignPayParams
    { sppRegAC = assetClassRegistrar
    , sppTreAC = assetClassTreasury
    }

  void $ Emulator.waitNSlots 1

-- +++ trace3 +++

-- Summary: Registrar is initialized with just one Signatory on record.  Said
-- signatory adds two others (only his vote is needed).  Later, three out of
-- four votes get accumulated in order to legimitately add one extra Signatory.
-- Log messages show the progress (signatories added and votes accumulated).
-- The end result is that four Signatories have been authorized.

trace3 :: EmulatorTrace ()
trace3 = do
  hF  <- activateContractWallet wF endpoints
  hS1 <- activateContractWallet wS1 endpoints
  hS2 <- activateContractWallet wS2 endpoints
  hS3 <- activateContractWallet wS3 endpoints

  -- | Initializes with one signatory registered (and treasury on stand-by).
  callEndpoint @"initializeScripts" hF $ InitializeParams
    { ipRegAC       = assetClassRegistrar
    , ipTreAC       = assetClassTreasury
    , ipSignatories = [pkhS1]
    , ipRatio       = (3,4)
    , ipDeadlineR   = deadline0
    , ipAmount      = 0
    , ipBeneficiary = Nothing
    , ipVotes       = []
    , ipDeadlineT   = deadline0
    }

  void $ Emulator.waitNSlots 1

  -- | Logs Registrar's state
  callEndpoint @"reportRegState" hF $ ReportParams
    { rpRegAC = assetClassRegistrar }

  void $ Emulator.waitNSlots 1

  -- | Proposes adding a two new signatories, which is immediately accepted (only
  -- one vote needed)
  callEndpoint @"proposeAddSig" hS1 $ PropAddSigParams
    { paspAddSignatories = [pkhS2, pkhS3]
    , paspRegAC          = assetClassRegistrar
    , paspDeadline       = deadline2
    }

  void $ Emulator.waitNSlots 1

  -- | Logs Registrar's state
  callEndpoint @"reportRegState" hF $ ReportParams
    { rpRegAC = assetClassRegistrar }

  void $ Emulator.waitNSlots 1

  -- | Proposes adding yet another signatory
  callEndpoint @"proposeAddSig" hS2 $ PropAddSigParams
    { paspAddSignatories = [pkhS4]
    , paspRegAC          = assetClassRegistrar
    , paspDeadline       = deadline2
    }

  void $ Emulator.waitNSlots 1

  -- | Logs Registrar's state
  callEndpoint @"reportRegState" hF $ ReportParams
    { rpRegAC = assetClassRegistrar }

  void $ Emulator.waitNSlots 1

  -- | Votes in favor of add proposal (second of 3 out of 4)
  callEndpoint @"approveAddSig" hS3 $ ApproveAddParams
    { aapRegAC = assetClassRegistrar }

  void $ Emulator.waitNSlots 1

  -- | Logs Registrar's state
  callEndpoint @"reportRegState" hF $ ReportParams
    { rpRegAC = assetClassRegistrar }

  void $ Emulator.waitNSlots 1

  -- | Votes in favor of add proposal (third of 3 out of 4)
  callEndpoint @"approveAddSig" hS1 $ ApproveAddParams
    { aapRegAC = assetClassRegistrar }

  void $ Emulator.waitNSlots 1

  -- | Logs final Registrar's state
  callEndpoint @"reportRegState" hF $ ReportParams
    { rpRegAC = assetClassRegistrar }

  void $ Emulator.waitNSlots 1
