{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
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

import           Prelude                     (Ordering (..), compare, read,
                                              show)

import           Codec.Serialise             (serialise)
import qualified Data.ByteString.Lazy        as LB
import qualified Data.ByteString.Short       as SBS

import           Cardano.Api.Shelley         (PlutusScript (..), PlutusScriptV1)
import           Ledger                      (Address (Address), Datum (..),
                                              DatumHash, PubKeyHash (..),
                                              ScriptContext (..), TokenName,
                                              TxInfo (..), TxOut, TxOutRef,
                                              Validator, ValidatorHash,
                                              ownCurrencySymbol, ownHash,
                                              pubKeyOutputsAt, txInInfoOutRef,
                                              txInInfoResolved, txInfoInputs,
                                              txInfoMint, txInfoSignatories,
                                              txOutAddress, txOutDatum,
                                              txSignedBy, unValidatorScript,
                                              validatorHash, valuePaidTo)
import qualified Ledger.Typed.Scripts        as Scripts
import           Ledger.Value                as Value (flattenValue, valueOf)
import qualified Plutus.V1.Ledger.Ada        as Ada (Ada (getLovelace),
                                                     fromValue, lovelaceOf)
import           Plutus.V1.Ledger.Credential (Credential (ScriptCredential))
import qualified Plutus.V1.Ledger.Scripts    as Plutus

import           PlutusTx.Builtins.Class
import           PlutusTx.Builtins
import qualified PlutusTx
import           PlutusTx.Prelude            as Plutus (Bool (..),
                                                        BuiltinString, Eq (..),
                                                        Integer, Maybe (..),
                                                        Ord, all, any, decodeUtf8,
                                                        appendString, filter,
                                                        fmap, fromInteger, head,
                                                        length, map, trace,
                                                        traceIfFalse, traceError,
                                                        traceIfTrue, ($), (%),
                                                        (&&), (*), (-), (.),
                                                        (<), (<=), (<>), (>),
                                                        (>=), (||))

import           Market.Types                (NFTSale (..), SaleAction (..))


{-# INLINABLE nftDatumHashToMNFTSale #-}
nftDatumHashToMNFTSale :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe NFTSale
nftDatumHashToMNFTSale txOut datumHashTOMDatumF = do
    datumHash <- txOutDatum txOut
    Datum datum <- datumHashTOMDatumF datumHash
    PlutusTx.fromBuiltinData datum

{-# INLINABLE mkBuyValidator #-}
mkBuyValidator :: NFTSale -> SaleAction -> ScriptContext -> Bool
mkBuyValidator nfts saleAction scriptCtx = case saleAction of
    Buy     -> traceIfFalse "There are multiple signatures." singleSignature
               -- && traceIfFalse "There are multiple tokens." singleTokenValue

               -- Useful for debugging.
               -- && traceIfTrue "Buyer ADA > NFT ADA." checkBuyerHasMoreADA
               -- && traceIfTrue "Buyer ADA < NFT ADA." checkBuyerHasLessADA

               && traceIfFalse "Buyer has not paid to NFT seller, input 0 ADA." checkBuyerHasPaid
               && traceIfFalse "The price of NFT does not match. Buyer ADA < NFT ADA." checkBuyerHasPaidCorrectPrice

               && traceIfFalse "There are not single purchases of the NFT." checkSingleBuy
               && traceIfFalse "NFT policy failed." checkNFTPolicy
               -- && uniqueToken -- TODO(KS): This is hard to validate, we need to figure that out from the blockchain.
    Close   ->
        let txSignedByAddress :: TxInfo
            txSignedByAddress = scriptContextTxInfo scriptCtx

            sellerAddress :: PubKeyHash
            sellerAddress = nSeller nfts

         in traceIfFalse (wrongSignatureTrace txSignedByAddress sellerAddress) (txSignedBy txSignedByAddress sellerAddress)
  where

    -- Issues with runtime representation of strings in the trace.
    wrongSignatureTrace :: TxInfo -> PubKeyHash -> BuiltinString
    wrongSignatureTrace _txSignedByAddress _sellerAddress =
        "NFT policy failed, NFT seller not signed."

    -- Checking that the price paid is the correct one.
    checkBuyerHasPaid :: Bool
    checkBuyerHasPaid = checkPrice (nSeller nfts)
      where
        -- We need to pay the seller the price of NFT.
        checkPrice :: PubKeyHash -> Bool
        checkPrice seller = Ada.fromValue (valuePaidTo scriptTxInfo seller) > 0

    -- Checking that the price paid is the correct one.
    checkBuyerHasLessADA :: Bool
    checkBuyerHasLessADA = checkPrice (nSeller nfts) (nPrice nfts)
      where
        -- We need to pay the seller the price of NFT.
        checkPrice :: PubKeyHash -> Integer -> Bool
        checkPrice seller price = Ada.fromValue (valuePaidTo scriptTxInfo seller) < Ada.lovelaceOf price

    -- Checking that the price paid is the correct one.
    checkBuyerHasPaidCorrectPrice :: Bool
    checkBuyerHasPaidCorrectPrice = checkPrice (nSeller nfts) (nPrice nfts)
      where
        -- We need to pay the seller the price of NFT.
        checkPrice :: PubKeyHash -> Integer -> Bool
        checkPrice seller price = Ada.fromValue (valuePaidTo scriptTxInfo seller) >= Ada.lovelaceOf price


    -- Get the transaction info.
    scriptTxInfo :: TxInfo
    scriptTxInfo = scriptContextTxInfo scriptCtx

    -- TODO(KS): Not total, but currently covered by the validation of the single signature.
    signatureKeyHash :: PubKeyHash
    signatureKeyHash = case txInfoSignatories scriptTxInfo of
            [pubKeyHash] -> pubKeyHash

    -- Checking we can have the UTxO consumed and a single NFT minted.
    checkNFTPolicy :: Bool
    checkNFTPolicy =
        let inputPendingTxOutRefs = map txInInfoOutRef (txInfoInputs scriptTxInfo)
         in any (\inputPendingTxOutRef -> mkPolicy inputPendingTxOutRef (nToken nfts)) inputPendingTxOutRefs
      where
        mkPolicy :: TxOutRef -> TokenName -> Bool
        mkPolicy oref tokenName =
            traceIfFalse "UTxO not consumed." hasUTxO &&
            traceIfFalse "wrong amount minted." checkMintedAmount
          where

            hasUTxO :: Bool
            hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs scriptTxInfo

            checkMintedAmount :: Bool
            checkMintedAmount =
                let flattenedVals = flattenValue (txInfoMint scriptTxInfo)
                 in all (\(currSymbol, tokenName', amount) -> currSymbol == ownCurrencySymbol scriptCtx && tokenName' == tokenName' && amount == 1) flattenedVals

    -- For now we have a single signature.
    singleSignature :: Bool
    singleSignature = length (txInfoSignatories scriptTxInfo) == 1

    -- Checking that there is a single token NFT sold to someone.
    singleTokenValue :: Bool
    singleTokenValue = valueOf (valuePaidTo scriptTxInfo signatureKeyHash) (nCurrency nfts) (nToken nfts) == 1

    -- Checking that there is a single output from script.
    checkSingleBuy :: Bool
    checkSingleBuy =
        -- txInInfoOutRef
        let inputPendingTx = map txInInfoResolved (txInfoInputs scriptTxInfo)
            txOutToScript = \inputTxOut -> txOutAddress inputTxOut == Address (ScriptCredential $ ownHash scriptCtx) Nothing
            txOutFromPendingTx = filter txOutToScript inputPendingTx

         in length txOutFromPendingTx == 1


data Sale
instance Scripts.ValidatorTypes Sale where
    type instance DatumType Sale = NFTSale
    type instance RedeemerType Sale = SaleAction


typedBuyValidator :: Scripts.TypedValidator Sale
typedBuyValidator = Scripts.mkTypedValidator @Sale
    ($$(PlutusTx.compile [|| mkBuyValidator ||]))
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @NFTSale @SaleAction


buyValidator :: Validator
buyValidator = Scripts.validatorScript typedBuyValidator

buyValidatorHash :: ValidatorHash
buyValidatorHash = validatorHash buyValidator

buyScript :: Plutus.Script
buyScript = Ledger.unValidatorScript buyValidator

buyScriptAsShortBs :: SBS.ShortByteString
buyScriptAsShortBs = SBS.toShort . LB.toStrict . serialise $ buyScript

apiBuyScript :: PlutusScript PlutusScriptV1
apiBuyScript = PlutusScriptSerialised buyScriptAsShortBs

