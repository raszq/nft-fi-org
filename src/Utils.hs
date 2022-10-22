{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils
    ( tryReadWalletId, unsafeReadWalletId
    , tryReadAddress, unsafeReadAddress
    , unsafeReadTxOutRef
    , unsafeReadPubKeyHash
    , unsafeReadCurrSymbol
    , writeJSON, writeUnit
    -- , contractActivationArgs
    , getCredentials, unsafePaymentPubKeyHash, unsafeStakePubKeyHash
    , cidToString
    , writeMintingPolicy
    , writeValidator
    , writeRedeemers
    , writeDatum
    , unsafeTokenNameToHex
    ) where

import           Cardano.Api                 as API
import           Cardano.Api.Shelley         (Address (..), PlutusScript (..))
import           Cardano.Crypto.Hash.Class   (hashToBytes)
import           Cardano.Ledger.BaseTypes    as Ledger
import           Cardano.Ledger.Credential   as Ledger
import           Cardano.Ledger.Crypto       (StandardCrypto)
import           Cardano.Ledger.Hashes       (ScriptHash (..))
import           Cardano.Ledger.Keys         (KeyHash (..))
import           Codec.Serialise             (serialise)
import           Data.Aeson                  (decode, encode)
import qualified Data.ByteString.Char8       as BS8
import qualified Data.ByteString.Lazy        as LBS
import qualified Data.ByteString.Short       as SBS
import           Data.Maybe                  (fromJust, fromMaybe)
import           Data.String                 (IsString (..))
import           Data.Text                   (Text, pack)
-- import           Plutus.PAB.Webserver.Types  (ContractActivationArgs (..))
import           Plutus.V1.Ledger.Credential as Plutus
import           Plutus.V1.Ledger.Crypto     as Plutus
import           Plutus.V1.Ledger.Value      (TokenName (..), CurrencySymbol(..))
import           PlutusTx                    (Data (..))
import qualified PlutusTx
import           PlutusTx.Builtins           (toBuiltin)
import           PlutusTx.Builtins.Internal  (BuiltinByteString (..), encodeUtf8)
import qualified Plutus.V2.Ledger.Api        as PlutusV2
import qualified Ledger                      as Plutus
import           Ledger.Bytes                (LedgerBytes(LedgerBytes), fromHex, fromBytes, getLedgerBytes)
import           Wallet.Emulator.Wallet      (WalletId (..), Wallet (..))
import           Wallet.Types                (ContractInstanceId (..))

dataToScriptData :: Data -> ScriptData
dataToScriptData (Constr n xs) = ScriptDataConstructor n $ dataToScriptData <$> xs
dataToScriptData (Map xs)      = ScriptDataMap [(dataToScriptData x, dataToScriptData y) | (x, y) <- xs]
dataToScriptData (List xs)     = ScriptDataList $ dataToScriptData <$> xs
dataToScriptData (I n)         = ScriptDataNumber n
dataToScriptData (B bs)        = ScriptDataBytes bs

credentialLedgerToPlutus :: Ledger.Credential a StandardCrypto -> Plutus.Credential
credentialLedgerToPlutus (ScriptHashObj (ScriptHash h)) = Plutus.ScriptCredential $ Plutus.ValidatorHash $ toBuiltin $ hashToBytes h
credentialLedgerToPlutus (KeyHashObj (KeyHash h))       = Plutus.PubKeyCredential $ Plutus.PubKeyHash $ toBuiltin $ hashToBytes h

stakeReferenceLedgerToPlutus :: Ledger.StakeReference StandardCrypto -> Maybe Plutus.StakingCredential
stakeReferenceLedgerToPlutus (StakeRefBase x)                   = Just $ StakingHash $ credentialLedgerToPlutus x
stakeReferenceLedgerToPlutus (StakeRefPtr (Ptr (SlotNo x) (Ledger.TxIx y) (Ledger.CertIx z))) = Just $ StakingPtr (fromIntegral x) (fromIntegral y) (fromIntegral z)
stakeReferenceLedgerToPlutus StakeRefNull                       = Nothing

tryReadAddress :: String -> Maybe Plutus.Address
tryReadAddress x = case deserialiseAddress AsAddressAny $ pack x of
    Nothing                                      -> Nothing
    Just (AddressByron _)                        -> Nothing
    Just (AddressShelley (ShelleyAddress _ p s)) -> Just Plutus.Address
        { Plutus.addressCredential        = credentialLedgerToPlutus p
        , Plutus.addressStakingCredential = stakeReferenceLedgerToPlutus s
        }

tryReadWalletId :: String -> Maybe WalletId
tryReadWalletId = decode . encode

tryReadCurrSymbol :: String -> Maybe CurrencySymbol
tryReadCurrSymbol s = case str2BBS s of
                     Left txt -> Nothing
                     Right bytes -> Just $ CurrencySymbol bytes

tryReadPubKeyHash :: String -> Maybe Plutus.PaymentPubKeyHash
--tryReadPubKeyHash s = Just $ Plutus.PaymentPubKeyHash "47e0d276a45734d3f03c76bfaa854b3a3a2beee7ad7103264dd5a040"
--tryReadPubKeyHash s = Just $ Plutus.PaymentPubKeyHash (PubKeyHash (getLedgerBytes (fromBytes (fromString s))))
tryReadPubKeyHash s = case str2BBS s of
                     Left txt -> Nothing
                     Right bytes -> Just $ Plutus.PaymentPubKeyHash $ PubKeyHash bytes
--tryReadPubKeyHash s = case pkhFromStr s of
--                      Left txt -> Nothing
--                      Right pkh -> Just (Plutus.PaymentPubKeyHash pkh)


unsafeReadWalletId :: String -> WalletId
unsafeReadWalletId s = fromMaybe (error $ "can't parse " ++ s ++ " as a WalletId") $ tryReadWalletId s

unsafeReadAddress :: String -> Plutus.Address
unsafeReadAddress s = fromMaybe (error $ "can't parse " ++ s ++ " as an address") $ tryReadAddress s

unsafeReadCurrSymbol :: String -> CurrencySymbol
unsafeReadCurrSymbol s = fromMaybe (error $ "can't parse " ++ s ++ " as a currency symbol") $ tryReadCurrSymbol s

unsafeReadPubKeyHash :: String -> Plutus.PaymentPubKeyHash
unsafeReadPubKeyHash s = fromMaybe (error $ "can't parse " ++ s ++ " as a pubkeyhash") $ tryReadPubKeyHash s

unsafeReadTxOutRef :: String -> Plutus.TxOutRef
unsafeReadTxOutRef s =
  let
    (x, _ : y) = span (/= '#') s
  in
    Plutus.TxOutRef
        { Plutus.txOutRefId  = fromString x
        , Plutus.txOutRefIdx = read y
        }

writeJSON :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeJSON file = LBS.writeFile file . encode . scriptDataToJson ScriptDataJsonDetailedSchema . dataToScriptData . PlutusTx.toData

writeUnit :: IO ()
writeUnit = writeJSON "testnet/unit.json" ()

-- contractActivationArgs :: WalletId -> a -> ContractActivationArgs a
-- contractActivationArgs wid a = ContractActivationArgs
--     { caID = a
--     , caWallet = Just $ Wallet {getWalletId = wid}
--     }

getCredentials :: Plutus.Address -> Maybe (Plutus.PaymentPubKeyHash, Maybe Plutus.StakePubKeyHash)
getCredentials (Plutus.Address x y) = case x of
    ScriptCredential _   -> Nothing
    PubKeyCredential pkh ->
      let
        ppkh = Plutus.PaymentPubKeyHash pkh
      in
        case y of
            Nothing                        -> Just (ppkh, Nothing)
            Just (Plutus.StakingPtr _ _ _) -> Nothing
            Just (StakingHash h)           -> case h of
                ScriptCredential _    -> Nothing
                PubKeyCredential pkh' -> Just (ppkh, Just $ Plutus.StakePubKeyHash pkh')

unsafePaymentPubKeyHash :: Plutus.Address -> Plutus.PaymentPubKeyHash
unsafePaymentPubKeyHash addr = maybe (error $ "script address " ++ show addr ++ " does not contain a payment key") fst $ getCredentials addr

unsafeStakePubKeyHash :: Plutus.Address -> Plutus.StakePubKeyHash
unsafeStakePubKeyHash addr = case getCredentials addr of
    Nothing           -> error $ "unexpected script address " ++ show addr
    Just (_, Nothing) -> error $ "addres " ++ show addr ++ " contains no stake component"
    Just (_, Just x)  -> x

cidToString :: ContractInstanceId -> String
cidToString = show . unContractInstanceId

writeMintingPolicyV1 :: FilePath -> Plutus.MintingPolicy -> IO (Either (FileError ()) ())
writeMintingPolicyV1 file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Plutus.getMintingPolicy

writeMintingPolicy :: FilePath -> Plutus.MintingPolicy -> IO (Either (FileError ()) ())
writeMintingPolicy file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . PlutusV2.getMintingPolicy


-- script :: PlutusV2.Script
-- script = PlutusV2.unMintingPolicyScript $ policy redeemer

-- {-
--     As a Short Byte String
-- -}

-- scriptSBS :: SBS.ShortByteString
-- scriptSBS = SBS.toShort . LBS.toStrict $ serialise script

-- {-
--     As a Serialised Script
-- -}

-- serialisedScript :: PlutusScript PlutusScriptV2
-- serialisedScript = PlutusScriptSerialised scriptSBS

-- writeSerialisedScript :: IO ()
-- writeSerialisedScript = void $ writeFileTextEnvelope "nft-mint-V2.plutus" Nothing serialisedScript







unsafeTokenNameToHex :: TokenName -> String
unsafeTokenNameToHex = BS8.unpack . serialiseToRawBytesHex . fromJust . deserialiseFromRawBytes AsAssetName . getByteString . unTokenName
  where
    getByteString (BuiltinByteString bs) = bs


writeValidatorV1 :: FilePath -> Plutus.Validator -> IO (Either (FileError ()) ())
writeValidatorV1 file = writeFileTextEnvelope @(PlutusScript PlutusScriptV1) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Plutus.unValidatorScript

writeValidator :: FilePath -> Plutus.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . PlutusV2.unValidatorScript


writeRedeemers :: PlutusTx.ToData a => String -> [(a, String)] -> IO ()
writeRedeemers filePrefix redeemers = writeAll redeemers 
    where
      writeAll [] = return ()
      writeAll (x : xs) = do 
        writeJSON (filePrefix ++ "Act" ++ (snd x) ++ ".json") (fst x)
        writeAll (xs)

writeDatum :: PlutusTx.ToData a => String -> a -> IO ()
writeDatum file dat = writeJSON file dat


--Could be useful later: https://cardano.stackexchange.com/questions/2578/find-pubkeyhash-from-string
pkhFromStr :: String -> Either Text PubKeyHash   
pkhFromStr s =         
    case fromHex (fromString s) of 
        Right (LedgerBytes bytes) -> Right $ PubKeyHash bytes 
        Left msg -> Left $ pack ("Could not convert from hex to bytes: " <> msg)

str2BBS :: String -> Either Text BuiltinByteString
str2BBS s =         
    case fromHex (fromString s) of 
        Right (LedgerBytes bytes) -> Right bytes 
        Left msg -> Left $ pack ("Could not convert from hex to bytes: " <> msg)