{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}

module SerialiseJSON
    ( testR
    , testD
    , writeData
    , toJsonString
    ) where

import           Data.Aeson           as Json (encode)
import qualified Data.ByteString.Lazy as LB


import           Cardano.Api          (ScriptDataJsonSchema (ScriptDataJsonDetailedSchema),
                                       scriptDataToJson)
import           Cardano.Api.Shelley  (fromPlutusData)
import qualified PlutusTx

import           Market.Types         (NFTSale (..), SaleAction (..))

-- This module is here to convert Haskell Data Types to JSON files, particularly used for Redeemer and Datum
-- To use this enter `cabal repl` ; `:l src/SerialiseJSON.hs` ; `testR` or `testD`


-- Constructs the JSON file for the Buy Redeemer constructor, used as input to --tx-in-redeemer-file
testR :: IO ()
testR = do
  writeData "update.json" Close
  putStrLn "Done"


nftEx :: NFTSale
nftEx = NFTSale
    { nPrice    = 5_000_000
    , nSeller   = "603517a452f0fec8c6fc816993f39eebf32fad8bd00cc1ffa4f2d02170"
    , nToken    = "NFT7"
    , nCurrency = "d9754fadcb64bca0ae4f5a4b447242538073a4e4bf1239ee25ee5e6c" -- policyID
    } -- This is an example to fill with real data
      -- The `nSeller` needs to be in Base16 format, not Bech32 (addr1...).
      -- To easily get the Base16 version, go to Cardanoscan.io, search the address in format addr1...
       -- The address is written in two formats, the first being Bech32 aka addr1... and the other (in light gray) being in Base16

-- Constructs the JSON file for the nftEx datum, used as input to --tx-in-datum-file
testD :: IO ()
testD = do
  writeData "datum.json" nftEx
  putStrLn "Done"
-- Datum also needs to be passed when sending the token to the script (aka putting for sale)
-- When doing this, the datum needs to be hashed, see Alonzo-purple exercise-solutions on how to hash a datum

writeData :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeData file isData = LB.writeFile file (toJsonString isData)

toJsonString :: PlutusTx.ToData a => a -> LB.ByteString
toJsonString =
  Json.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . fromPlutusData
    . PlutusTx.toData
