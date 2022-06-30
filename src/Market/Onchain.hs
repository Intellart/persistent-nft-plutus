{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}

module Market.Onchain
    ( apiBuyScript
    , buyScriptAsShortBs
    , typedBuyValidator
    , Sale
    , buyValidator
    , buyValidatorHash
    , nftDatumHashToMNFTSale
    ) where

import           Codec.Serialise             (serialise)
import qualified Data.ByteString.Lazy        as LB
import qualified Data.ByteString.Short       as SBS

import           Cardano.Api.Shelley         (PlutusScript (..), PlutusScriptV1)
import           Ledger                      (Address (Address), Datum (..),
                                              DatumHash, PubKeyHash (..),
                                              ScriptContext (scriptContextTxInfo),
                                              TxInfo, TxOut, Validator,
                                              ValidatorHash, ownHash,
                                              txInInfoResolved, txInfoInputs,
                                              txInfoSignatories, txOutAddress,
                                              txOutDatum, txSignedBy,
                                              unValidatorScript, validatorHash,
                                              valuePaidTo)
import qualified Ledger.Typed.Scripts        as Scripts
import           Ledger.Value                as Value (valueOf)
import qualified Plutus.V1.Ledger.Ada        as Ada (Ada (getLovelace), lovelaceOf,
                                                     fromValue)
import           Plutus.V1.Ledger.Credential (Credential (ScriptCredential))
import qualified Plutus.V1.Ledger.Scripts    as Plutus
import qualified PlutusTx
import           PlutusTx.Prelude            as Plutus (Bool (..), Eq ((==)), (<=),
                                                        Integer, Maybe (..),
                                                        fromInteger, length,
                                                        map, ($), (%), (&&),
                                                        (*), (-), (.), (>=),
                                                        (||))


import           Market.Types                (MarketParams (..), NFTSale (..),
                                              SaleAction (..))


{-# INLINABLE nftDatumHashToMNFTSale #-}
nftDatumHashToMNFTSale :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe NFTSale
nftDatumHashToMNFTSale txOut datumHashTOMDatumF = do
    datumHash <- txOutDatum txOut
    Datum datum <- datumHashTOMDatumF datumHash
    PlutusTx.fromBuiltinData datum

{-# INLINABLE mkBuyValidator #-}
mkBuyValidator :: MarketParams -> NFTSale -> SaleAction -> ScriptContext -> Bool
mkBuyValidator mp nfts saleAction ctx = case saleAction of
    Buy     -> checkFee (nPrice nfts)
               && (valueOf (valuePaidTo info sig) (nCurrency nfts) (nToken nfts) == 1)
               && checkSellerOut (nSeller nfts) (nPrice nfts)
               && checkSingleBuy
    Close   -> txSignedBy (scriptContextTxInfo ctx) (nSeller nfts)

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    sig :: PubKeyHash
    sig = case txInfoSignatories info of
            [pubKeyHash] -> pubKeyHash

    checkSingleBuy :: Bool
    checkSingleBuy = let is = [ i | i <- map txInInfoResolved (txInfoInputs info), txOutAddress i == Address (ScriptCredential $ ownHash ctx) Nothing ] in
        length is == 1

    -- TODO(KS): Fix this to be precise
    checkFee :: Integer -> Bool
    checkFee price = Ada.fromValue (valuePaidTo info (feeAddr mp)) <= Ada.lovelaceOf price

    checkSellerOut :: PubKeyHash -> Integer -> Bool
    checkSellerOut seller price = Ada.fromValue (valuePaidTo info seller) <= Ada.lovelaceOf price


data Sale
instance Scripts.ValidatorTypes Sale where
    type instance DatumType Sale    = NFTSale
    type instance RedeemerType Sale = SaleAction


typedBuyValidator :: MarketParams -> Scripts.TypedValidator Sale
typedBuyValidator mp = Scripts.mkTypedValidator @Sale
    ($$(PlutusTx.compile [|| mkBuyValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode mp)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @NFTSale @SaleAction


buyValidator :: MarketParams -> Validator
buyValidator = Scripts.validatorScript . typedBuyValidator

buyValidatorHash :: MarketParams -> ValidatorHash
buyValidatorHash = validatorHash . buyValidator

buyScript :: MarketParams -> Plutus.Script
buyScript = Ledger.unValidatorScript . buyValidator

buyScriptAsShortBs :: MarketParams -> SBS.ShortByteString
buyScriptAsShortBs = SBS.toShort . LB.toStrict . serialise . buyScript

apiBuyScript :: MarketParams -> PlutusScript PlutusScriptV1
apiBuyScript = PlutusScriptSerialised . buyScriptAsShortBs

