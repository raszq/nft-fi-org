module Main
    ( main
    ) where

import Control.Exception        (throwIO)
import Data.String              (IsString (..))
import System.Environment       (getArgs)
import CustodialAssetManager    (validator, ContractParam(..), RedeemAction(..))
import Utils                    (unsafeReadPubKeyHash, writeValidator, writeRedeemers, unsafeReadCurrSymbol)

main :: IO ()
main = do
    [filePrefix, pkh', nftPolId'] <- getArgs
    let pkh      = unsafeReadPubKeyHash pkh'
        nftPolId = unsafeReadCurrSymbol nftPolId'
        param    = ContractParam
                    { firmOwners  = [pkh]
                    , nftPolId    = nftPolId
                    } 
    e <- writeValidator (filePrefix ++ ".plutus") $ validator $ param
    case e of
        Left err -> throwIO $ userError $ show err
        Right () -> return ()
    writeRedeemers filePrefix myRedeemers

myRedeemers ::[(RedeemAction, String)]
myRedeemers = [(SpendFund, "SpendFund"), (SpendFee, "SpendFee"), (NftSign, "NftSign"), (NftChangeOwner, "NftChangeOwner"), (Revoke, "Revoke")]

