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
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module CustodialAssetManager where

-- import           Control.Monad        hiding (fmap)
-- import           Data.Aeson           (ToJSON, FromJSON)
-- import           Data.Map             as Map
-- import           Data.Text            (Text)
-- import           Data.Void            (Void)
import           GHC.Generics                           (Generic)
-- import           Plutus.Contract
import           PlutusTx             
import           PlutusTx.Prelude                       hiding (Semigroup(..), unless)
-- import           Ledger               hiding (singleton)
-- import           Ledger.Constraints   (TxConstraints)
-- import qualified Ledger.Constraints   as Constraints
-- import qualified Ledger.Typed.Scripts as Scripts
import qualified Plutus.Script.Utils.V1.Scripts       as PSU.V1
import qualified Plutus.Script.Utils.V1.Typed.Scripts as PSU.V1
import qualified Plutus.Script.Utils.V2.Typed.Scripts as PSU.V2
-- import           Ledger.Ada                             as Ada
-- import           Ledger.Value                           as Value
-- import           Plutus.V1.Ledger.Api                 
import           Plutus.V1.Ledger.Value
import           Ledger.Address
import           Plutus.V2.Ledger.Api                   
import           Plutus.V2.Ledger.Contexts              
import           Prelude                                (IO, Semigroup (..), Show (..), String)
import           Text.Printf                            (printf)


data ContractParam = ContractParam
    { firmOwners  :: [PaymentPubKeyHash]
    , nftPolId    :: CurrencySymbol
    } deriving Show

PlutusTx.makeLift ''ContractParam


data FundType = Fee | IdentNft | Funds deriving (Show)
PlutusTx.unstableMakeIsData ''FundType
instance Eq FundType where
  (==) Fee Fee              = True
  (==) IdentNft IdentNft    = True
  (==) Funds Funds          = True
  (==) _ _                  = False

data PermissionDatum = PermissionDatum
    { nftSigners  :: [TokenName] --token asset names
    , nftOwner    :: [PaymentPubKeyHash]
    , fundType    :: FundType
    } deriving Show

PlutusTx.unstableMakeIsData ''PermissionDatum
instance Eq PermissionDatum where
    {-# INLINABLE (==) #-}
    a == b =  (nftSigners a == nftSigners b)
           && (nftOwner   a == nftOwner   b)
           && (fundType   a == fundType   b) 


data RedeemAction = SpendFund | SpendFee | NftSign | NftChangeOwner | Revoke
PlutusTx.unstableMakeIsData ''RedeemAction
PlutusTx.makeLift ''RedeemAction


{-# INLINABLE mkCustodialValidator #-}
mkCustodialValidator :: ContractParam -> PermissionDatum -> RedeemAction -> ScriptContext -> Bool
mkCustodialValidator p dat action ctx = case action of
    SpendFund ->
        traceIfFalse "Nft parent owners not signed by consuming their utxos"        signedByNftSigners 
    SpendFee ->
        traceIfFalse "No NFT has signed by consuming"                               nftWasConsumed
    NftSign ->
        traceIfFalse "NFT wallet owner not signed"                                  signedByAnyWalletOwner                      &&
        traceIfFalse "Current Utxo Asset not deposited back"                        ownInputStays                               &&
        traceIfFalse "Datum must not change"                                        (ownOutDatum == dat)
    NftChangeOwner ->
        traceIfFalse "Nft parent owners not signed by consuming their utxos"        signedByNftSigners                          &&
        traceIfFalse "Current Utxo Asset not deposited back"                        ownInputStays                               &&
        traceIfFalse "Only owner datum should change"                               ((nftSigners ownOutDatum == nftSigners dat)
                                                                                    && (fundType ownOutDatum == fundType dat))
    Revoke ->
        traceIfFalse "Signed by any of root owners"                                 signedByAnyRootOwner
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx


    nftWasConsumed :: Bool
    nftWasConsumed = any isPresent (getContinuingOutputs ctx)
        where
            isPresent :: TxOut -> Bool
            isPresent outUtxo = case (symbols (txOutValue outUtxo)) of
                [curr]  -> curr == nftPolId p
                _       -> False

    findNftInScript :: TokenName -> Bool
    findNftInScript tn = any isPresent (txInfoInputs info)
        where
            isPresent :: TxInInfo -> Bool
            isPresent inUtxo = (valueOf ((txOutValue  . txInInfoResolved) inUtxo) (nftPolId p) tn) == 1

    findNftOutScript :: TokenName -> Bool
    findNftOutScript tn = any isPresent (getContinuingOutputs ctx)
        where
            isPresent :: TxOut -> Bool
            isPresent outUtxo = (valueOf (txOutValue outUtxo) (nftPolId p) tn) == 1

    signedByNftSigners :: Bool
    signedByNftSigners = let signers = nftSigners dat
        in 
            all (\nftSigner -> (findNftInScript nftSigner) && (findNftOutScript nftSigner)) signers   &&
            length signers /= 0

    
    ownOutput :: TxOut
    ownInputStays :: Bool
    (ownOutput, ownInputStays) = case (findOwnInput ctx) of
        Just txInInfo   -> let
                xs = [txOut | txOut <- getContinuingOutputs ctx, txOutValue txOut == txOutValue (txInInfoResolved txInInfo)]
            in
                case xs of
                    [txOut] -> (txOut, True)
                    _       -> traceError "Can't find own input at continuing outputs"
        Nothing         -> traceError "Can't find own input Utxo"

    ownOutDatum :: PermissionDatum
    ownOutDatum = case txOutDatum ownOutput of
        OutputDatumHash dHash   -> case findDatum dHash info of
                                    Just (Datum d)  -> case PlutusTx.fromBuiltinData d of
                                                            Just d' -> d'
                                                            Nothing  -> traceError "error decoding datum on output"
                                    Nothing         -> traceError "No own output datum found"
        _                       -> traceError "hash not found on own outdatum"


    signedByAnyWalletOwner :: Bool
    signedByAnyWalletOwner = any (\owner -> txSignedBy info $ unPaymentPubKeyHash owner) (nftOwner dat)

    signedByAnyRootOwner :: Bool
    signedByAnyRootOwner = any (\owner -> txSignedBy info $ unPaymentPubKeyHash owner) (firmOwners p)

-- data Custodial
-- instance PSU.V1.ValidatorTypes Custodial where
--     type instance DatumType Custodial = PermissionDatum
--     type instance RedeemerType Custodial = RedeemAction

-- typedValidator :: ContractParam -> PSU.V1.TypedValidator Custodial
-- typedValidator tv = PSU.V1.mkTypedValidator @Custodial
--     ($$(PlutusTx.compile [|| mkCustodialValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode tv)
--     $$(PlutusTx.compile [|| wrap ||])
--   where
--     wrap = Scripts.wrapValidator @PermissionDatum @RedeemAction



--This ff method saved just about 350/2 bytescompared to using TypedValidator 

{-# INLINABLE mkWrappedCustodialValidator #-}
mkWrappedCustodialValidator :: ContractParam ->  BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedCustodialValidator constructor   d r c = check $ mkCustodialValidator constructor (parseData d "CustodialValidator: Invalid datum") (parseData r "CustodialValidator: Invalid redeemer") (PlutusTx.unsafeFromBuiltinData c)
  where
    parseData d s = case PlutusTx.fromBuiltinData  d of
      Just d -> d
      _      -> traceError s

configurableCustodialValidator ::  ContractParam -> Validator
configurableCustodialValidator constructor = mkValidatorScript  $
            $$(PlutusTx.compile [|| mkWrappedCustodialValidator ||])
            `PlutusTx.applyCode` PlutusTx.liftCode constructor

validator :: ContractParam -> Validator
validator = configurableCustodialValidator


createDatum :: FundType -> [TokenName] -> [PaymentPubKeyHash] -> PermissionDatum
createDatum f [] [] = PermissionDatum 
                    { nftSigners    = []
                    , nftOwner      = []
                    , fundType      = f
                    }
createDatum f [] p = PermissionDatum 
                    { nftSigners    = []
                    , nftOwner      = p
                    , fundType      = f
                    }
createDatum f t [] = PermissionDatum 
                    { nftSigners    = t
                    , nftOwner      = []
                    , fundType      = f
                    }
createDatum f t p = PermissionDatum 
                    { nftSigners    = t
                    , nftOwner      = p
                    , fundType      = f
                    }