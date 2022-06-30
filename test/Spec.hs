{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Main
    ( main
    ) where

import           Control.Lens           ((&), (.~))
import           Control.Monad          (void)
import           Data.Default           (def)
import           Prelude

import           Test.Tasty             (defaultMain, testGroup)

import           Ledger.Value           as Value (AssetClass (..), assetClass,
                                                  assetClassValue, singleton,
                                                  valueOf)
import qualified Plutus.V1.Ledger.Ada   as Ada (lovelaceValueOf)


import qualified Data.Map               as Map

import           Playground.Types       (PayToWalletParams (PayToWalletParams),
                                         payTo, value)

import           Test.Tasty             (TestTree, testGroup)
import           Test.Tasty.HUnit       (assertEqual, testCase)

import           Wallet.Emulator.Types  (WalletNumber (WalletNumber))
import           Wallet.Emulator.Wallet (Wallet, knownWallet, walletPubKeyHash)

import           Market.Offchain        (endpoints)
import           Market.Types           (BuyParams (..), StartParams (..))

import           Plutus.Contract.Test
import           Plutus.Trace.Emulator  as Emulator (EmulatorConfig (..),
                                                     EmulatorTrace,
                                                     activateContractWallet,
                                                     callEndpoint,
                                                     runEmulatorTraceIO',
                                                     waitNSlots)


main :: IO ()
main =
    defaultMain $
    testGroup
        "all tests"
        [ simpleBuyTest
        --, simpleBuyTwoTokensTest
        --, multipleTokensInExistanceTest
        ]

nftEx1 :: StartParams
nftEx1 = StartParams
    { sPrice = 1_000_000
    , sTn    = "NFT1"
    , sCs    = "0001"
    }

nftEx2 :: StartParams
nftEx2 = StartParams
    { sPrice = 1_000_000
    , sTn    = "NFT2"
    , sCs    = "0001"
    }

nftEx1' :: BuyParams
nftEx1' = BuyParams
    { bTn = sTn nftEx1
    , bCs = sCs nftEx1
    }

nftEx2' :: BuyParams
nftEx2' = BuyParams
    { bTn = sTn nftEx2
    , bCs = sCs nftEx2
    }

testAssetClassNft1 :: AssetClass
testAssetClassNft1 = assetClass (sCs nftEx1) (sTn nftEx1)

testAssetClassNft2 :: AssetClass
testAssetClassNft2 = assetClass (sCs nftEx2) (sTn nftEx2)


-- We start the sale of NFT1 from wallet 3 and wallet 4 buys it.
-- We can notice that the fee has been deducted from the seller.
simpleBuyTest :: TestTree
simpleBuyTest = checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "token simple buy trace"
    walletsChange
    testTrace
  where
    testTrace :: EmulatorTrace ()
    testTrace = do
        h3 <- activateContractWallet w3 endpoints
        h4 <- activateContractWallet w4 endpoints

        callEndpoint @"start" h3 nftEx1
        void $ Emulator.waitNSlots 1
        callEndpoint @"buy" h4 (nftEx1')
        void $ Emulator.waitNSlots 1

    walletsChange :: TracePredicate
    walletsChange =
             walletFundsChange w3 (Ada.lovelaceValueOf (980_000) <> assetClassValue testAssetClassNft1 (-1))
        .&&. walletFundsChange w4 (Ada.lovelaceValueOf (-1_000_000) <> assetClassValue testAssetClassNft1 1)

    emCfg :: EmulatorConfig
    emCfg = EmulatorConfig (Left dist) def def
      where
        dist = Map.fromList
            [ (w3, Ada.lovelaceValueOf 10_000_000
                <> Value.singleton (sCs nftEx1) (sTn nftEx1) 1
                <> Value.singleton (sCs nftEx2) (sTn nftEx2) 1)
            , (w4, Ada.lovelaceValueOf 10_000_000)
            ]


-- We start the sale of NFT1 from wallet 3 and wallet 4 buys it.
-- Then we start the sale of NFT2 from wallet 2 and wallet 1 buys it.
-- We can notice that the fee has been deducted from the seller.
simpleBuyTwoTokensTest :: TestTree
simpleBuyTwoTokensTest = checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "token simple buy two tokens from two wallets trace"
    walletsChange
    testTrace
  where
    testTrace :: EmulatorTrace ()
    testTrace = do
        h1 <- activateContractWallet w1 endpoints
        h2 <- activateContractWallet w2 endpoints
        h3 <- activateContractWallet w3 endpoints
        h4 <- activateContractWallet w4 endpoints

        callEndpoint @"start" h3 nftEx1
        void $ Emulator.waitNSlots 1
        callEndpoint @"buy" h4 (nftEx1')
        void $ Emulator.waitNSlots 1

        callEndpoint @"start" h2 nftEx2
        void $ Emulator.waitNSlots 1
        callEndpoint @"buy" h1 (nftEx2')
        void $ Emulator.waitNSlots 1

    walletsChange :: TracePredicate
    walletsChange =
             walletFundsChange w3 (Ada.lovelaceValueOf (980_000) <> assetClassValue testAssetClassNft1 (-1))
        .&&. walletFundsChange w4 (Ada.lovelaceValueOf (-1_000_000) <> assetClassValue testAssetClassNft1 1)

        .&&. walletFundsChange w2 (Ada.lovelaceValueOf (980_000) <> assetClassValue testAssetClassNft2 (-1))
        .&&. walletFundsChange w1 (Ada.lovelaceValueOf (-960_000) <> assetClassValue testAssetClassNft2 1)

    emCfg :: EmulatorConfig
    emCfg = EmulatorConfig (Left dist) def def
      where
        dist = Map.fromList
            [ (w1, Ada.lovelaceValueOf 10_000_000)
            , (w2, Ada.lovelaceValueOf 10_000_000
                <> Value.singleton (sCs nftEx2) (sTn nftEx2) 1)
            , (w3, Ada.lovelaceValueOf 10_000_000
                <> Value.singleton (sCs nftEx1) (sTn nftEx1) 1)
            , (w4, Ada.lovelaceValueOf 10_000_000)
            ]

-- We start the sale of NFT1 from wallet 3, but the NFT is not unique so the constraints are invalidated.
-- TODO(KS): Map to error!
multipleTokensInExistanceTest :: TestTree
multipleTokensInExistanceTest = checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "multiple tokens buy trace"
    walletsChange
    testTrace
  where
    testTrace :: EmulatorTrace ()
    testTrace = do
        h3 <- activateContractWallet w3 endpoints
        h4 <- activateContractWallet w4 endpoints

        callEndpoint @"start" h3 nftEx1
        void $ Emulator.waitNSlots 1
        callEndpoint @"buy" h4 (nftEx1')
        void $ Emulator.waitNSlots 1

    walletsChange :: TracePredicate
    walletsChange = pure True

    emCfg :: EmulatorConfig
    emCfg = EmulatorConfig (Left dist) def def
      where
        dist = Map.fromList
            [ (knownWallet 3, Ada.lovelaceValueOf 10_000_000
                <> Value.singleton (sCs nftEx2) (sTn nftEx2) 1
                <> Value.singleton (sCs nftEx1) (sTn nftEx1) 2)
            , (knownWallet 4, Ada.lovelaceValueOf 10_000_000)
            ]


