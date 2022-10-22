module Main
    ( main
    ) where

import Control.Exception    (throwIO)
import Data.String          (IsString (..))
import System.Environment   (getArgs)
import NftMintFactory       (policy)
import Utils                (unsafeReadPubKeyHash, writeMintingPolicy)

main :: IO ()
main = do
    [file, pkh'] <- getArgs
    let pkh = unsafeReadPubKeyHash pkh'
        --pkh = fromString pkh'
        p   = policy pkh
    e <- writeMintingPolicy file p
    case e of
        Left err -> throwIO $ userError $ show err
        Right () -> return ()