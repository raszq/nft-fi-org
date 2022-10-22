module Main
    ( main
    ) where

import Control.Exception        (throwIO)
import Data.String              (IsString (..))
import System.Environment       (getArgs)
import CustodialAssetManager    (PermissionDatum(..), FundType(..), createDatum)
import Utils                    (unsafeReadPubKeyHash, writeDatum)

main :: IO ()
main = do
    [file, tn', pkh'] <- getArgs
    let pkh = case pkh' of
                [] -> []
                _  -> [unsafeReadPubKeyHash pkh']
        tn  = case tn' of
                [] -> []
                _  -> [fromString tn']
        dat = createDatum IdentNft tn pkh
    writeDatum file dat



