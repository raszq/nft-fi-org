{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module NftMintFactory 
    ( policy
    , curSymbol
    ) where

import qualified PlutusTx
import           PlutusTx.Prelude                       hiding (Semigroup(..), unless)
-- import           Ledger                                 hiding (mint, singleton)
import qualified Ledger.Typed.Scripts                   as Scripts
import qualified Plutus.Script.Utils.V2.Typed.Scripts   as PSU.V2
import           Plutus.Script.Utils.V2.Scripts         (scriptCurrencySymbol)
-- import           Ledger.Value                        as Value
import           Plutus.V1.Ledger.Value
import qualified Plutus.V2.Ledger.Api                   as PlutusV2
import           Plutus.V2.Ledger.Contexts
import           Ledger.Address

{-# INLINABLE mkPolicy #-}
mkPolicy :: PaymentPubKeyHash -> () -> ScriptContext -> Bool
mkPolicy pkh () ctx = traceIfFalse "wrong mint data"       (checkMintedAmount)                            &&
                      traceIfFalse "signed by firm"        (txSignedBy info $ unPaymentPubKeyHash pkh)
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        checkMintedAmount :: Bool
        checkMintedAmount = case flattenValue (txInfoMint info) of
            [(_, tn, amt)] -> traceIfFalse "Mint amount should be 1"                         (amt == 1)  &&
                              traceIfFalse "Token Name should contain AAA"                   (takeByteString 3 (unTokenName tn) == "AAA")
            _              -> traceError "No single element mint output found..."

policy :: PaymentPubKeyHash -> Scripts.MintingPolicy
policy mp = PlutusV2.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode`
     PlutusTx.liftCode mp
  where
    wrap mp' = PSU.V2.mkUntypedMintingPolicy $ mkPolicy mp'

-- policy :: PaymentPubKeyHash -> Scripts.MintingPolicy
-- policy pkh = mkMintingPolicyScript $
--     $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
--     `PlutusTx.applyCode`
--     PlutusTx.liftCode pkh

curSymbol :: PaymentPubKeyHash -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy
