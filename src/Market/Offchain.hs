{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Market.Offchain
    ( endpoints
    )
    where

import           Control.Monad        (forever, void)
import           Data.Aeson           (ToJSON)
import qualified Data.Map             as Map
import           Data.Monoid          as Mnd (mconcat, (<>))
import           Data.Text            (Text)
import           Prelude              (Float, String, and, ceiling, const,
                                       fromIntegral, show, (*), (-), (/))


import           Ledger               (ChainIndexTxOut, CurrencySymbol,
                                       Datum (Datum), Redeemer (Redeemer),
                                       TokenName, TxOut (txOutValue), TxOutRef,
                                       ValidatorHash, getCardanoTxId,
                                       pubKeyHash, pubKeyHashAddress,
                                       scriptAddress, toTxOut)
import           Ledger.Constraints   as Constraints (mustPayToOtherScript,
                                                      mustPayToPubKey,
                                                      mustPayToTheScript,
                                                      mustSpendScriptOutput,
                                                      otherScript,
                                                      typedValidatorLookups,
                                                      unspentOutputs)
import           Ledger.Value         as Value (singleton, valueOf)
import           Plutus.ChainIndex.Tx (ChainIndexTx (_citxData))
import           Plutus.Contract      as Contract (AsContractError, Contract,
                                                   Promise (awaitPromise),
                                                   awaitTxConfirmed, endpoint,
                                                   handleError, logError,
                                                   logInfo, ownPubKeyHash,
                                                   select, selectList,
                                                   submitTxConstraintsWith,
                                                   utxosAt, utxosTxOutTxAt)
import qualified Plutus.V1.Ledger.Ada as Ada (lovelaceValueOf)
import qualified PlutusTx
import           PlutusTx.Prelude     as Plutus (Bool, Eq ((==)), Integer,
                                                 Maybe (..), isJust, map,
                                                 return, ($), (++), (<$>))

import           Market.Onchain       as O2 (Sale, buyValidator,
                                             buyValidatorHash,
                                             nftDatumHashToMNFTSale,
                                             typedBuyValidator)
import           Market.Types         (BuyParams (..), MarketParams (..),
                                       NFTSale (..), SaleAction (..),
                                       SaleSchema, StartParams (..))
import           Utility              (mp, wallet, walletPubKeyHash)


startSale :: StartParams -> Contract w SaleSchema Text ()
startSale sp = do
    pkh <- Contract.ownPubKeyHash
    utxos <- utxosAt (pubKeyHashAddress pkh)
    let val     = Value.singleton (sCs sp) (sTn sp) 1
        nfts    = NFTSale { nSeller = pkh, nToken = sTn sp, nCurrency = sCs sp, nPrice = sPrice sp }
        lookups = Constraints.unspentOutputs utxos <>
                  Constraints.typedValidatorLookups (O2.typedBuyValidator mp)
        tx      = Constraints.mustPayToTheScript nfts val
    ledgerTx <- submitTxConstraintsWith @O2.Sale lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    Contract.logInfo @String "Starting sale, transaction confirmation"


buy :: BuyParams -> Contract w SaleSchema Text ()
buy bp = do
    pkh <- Contract.ownPubKeyHash
    sale <- findSale (bCs bp, bTn bp)
    case sale of
        Nothing -> Contract.logError @String "No sale found"
        Just (oref, o, nfts) -> do
            let r       = Redeemer $ PlutusTx.toBuiltinData Buy
                val     = Value.singleton (nCurrency nfts) (nToken nfts) 1

            let lookups = Constraints.typedValidatorLookups (O2.typedBuyValidator mp) <>
                          Constraints.unspentOutputs (Map.singleton oref o)   <>
                          Constraints.otherScript (O2.buyValidator mp)
                tx      = Constraints.mustSpendScriptOutput oref r           <>
                          Constraints.mustPayToPubKey pkh val

            ledgerTx <- submitTxConstraintsWith lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String "Buying, transaction confirmation"


update :: (BuyParams, Integer) -> Contract w SaleSchema Text ()
update (bp, newprice) = do
    sale <- findSale (bCs bp, bTn bp)
    case sale of
        Nothing -> Contract.logError @String "Updating, no sale found"
        Just (oref, o, nfts) -> do
            let r       = Redeemer $ PlutusTx.toBuiltinData Close
                val     = Value.singleton (nCurrency nfts) (nToken nfts) 1
                nfts'   = nfts { nPrice = newprice }
                lookups = Constraints.typedValidatorLookups (O2.typedBuyValidator mp) <>
                          Constraints.otherScript (O2.buyValidator mp)                <>
                          Constraints.unspentOutputs (Map.singleton oref o)
                tx      = Constraints.mustSpendScriptOutput oref r <>
                          Constraints.mustPayToTheScript nfts' val
            ledgerTx <- submitTxConstraintsWith lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String "Price updated, transaction confirmed"


close :: BuyParams -> Contract w SaleSchema Text ()
close bp = do
    sale <- findSale (bCs bp, bTn bp)
    case sale of
        Nothing -> Contract.logError @String "Closing, no sale found"
        Just (oref, o, nfts) -> do
            let r       = Redeemer $ PlutusTx.toBuiltinData Close
                val     = Value.singleton (nCurrency nfts) (nToken nfts) 1 -- <> Ada.lovelaceValueOf 1724100
                lookups = Constraints.typedValidatorLookups (O2.typedBuyValidator mp) <>
                          Constraints.otherScript (O2.buyValidator mp)                <>
                          Constraints.unspentOutputs (Map.singleton oref o)
                tx      = Constraints.mustSpendScriptOutput oref r      <>
                          Constraints.mustPayToPubKey (nSeller nfts) val
            ledgerTx <- submitTxConstraintsWith lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String "Closing, transaction confirmation"


findSale :: (AsContractError e, ToJSON e) => (CurrencySymbol, TokenName) -> Contract w SaleSchema e (Maybe (TxOutRef, ChainIndexTxOut, NFTSale))
findSale (currSymbol, tokenName) = do
    -- We first filter out that the contract contains UTXOs that have tokens (sale started)
    utxos <- Map.filter filterSingleTokens <$> utxosTxOutTxAt (scriptAddress $ O2.buyValidator mp)
    return $ case Map.toList utxos of
        [(txOutRef, (chainIndexTxOut, citx))] -> do
            --Contract.logInfo @String "Found sale in contract UTXO."
            nfts <- nftDatumHashToMNFTSale (toTxOut chainIndexTxOut) $ \datumHash -> Map.lookup datumHash $ _citxData citx
            Just (txOutRef, chainIndexTxOut, nfts)

        _           -> do
            --Contract.logInfo @String "Cannot find sale in contract UTXO."
            Nothing

  where
    filterSingleTokens :: (ChainIndexTxOut, Plutus.ChainIndex.Tx.ChainIndexTx) -> Bool
    filterSingleTokens (chainIndexTxOut, _) = valueOf (txOutValue $ toTxOut chainIndexTxOut) currSymbol tokenName == 1


endpoints :: Contract () SaleSchema Text ()
endpoints = selectList [startEndpoint, buyEndpoint, closeEndpoint]

startEndpoint :: Promise () SaleSchema Text ()
startEndpoint = endpoint @"start" $ \nfts -> startSale nfts

buyEndpoint :: Promise () SaleSchema Text ()
buyEndpoint = endpoint @"buy" $ \buyParams -> buy buyParams

closeEndpoint :: Promise () SaleSchema Text ()
closeEndpoint = endpoint @"close" $ \nfts -> close nfts

