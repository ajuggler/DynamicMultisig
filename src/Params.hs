{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE NumericUnderscores #-}

module Params where

import Ledger                  (minAdaTxOut, POSIXTime(..))
import Ledger.Value
import Ledger.Ada
import PlutusTx.Prelude


-- TOKENS --

thTkRegistrarCS :: CurrencySymbol
thTkRegistrarCS = "a1ffa2ffa3ffa4ffa5ffa6ffa7ffb1ffb2ffb3ffb4ffb5ffb6ffb7ff"

thTkRegistrarNm :: TokenName
thTkRegistrarNm = "REGISTRAR TOKEN"

thTkTreasuryCS :: CurrencySymbol
thTkTreasuryCS = "c1ffc2ffc3ffc4ffc5ffc6ffc7ffd1ffd2ffd3ffd4ffd5ffd6ffd7ff"

thTkTreasuryNm :: TokenName
thTkTreasuryNm = "TREASURY TOKEN"

assetClassRegistrar, assetClassTreasury :: AssetClass
assetClassRegistrar = assetClass thTkRegistrarCS thTkRegistrarNm
assetClassTreasury  = assetClass thTkTreasuryCS thTkTreasuryNm


-- ALIASES --

type Ratio = (Integer, Integer)


-- GLOBAL PARAMETERS --

-- | Begining of time.
beginTime :: POSIXTime
beginTime = POSIXTime {getPOSIXTime = 1596059092000}

valMinAda :: Value
valMinAda = toValue minAdaTxOut

treasuryFunding :: Value
treasuryFunding = lovelaceValueOf 1000_000_000

