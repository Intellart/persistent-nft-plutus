{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}

module Market.Types
    ( SaleAction (..)
    , SaleSchema
    , StartParams (..)
    , BuyParams (..)
    , NFTSale (..)
    )
    where

import           Data.Aeson       (FromJSON, ToJSON)
import           GHC.Generics     (Generic)
import           Prelude          (Show (..))
import qualified Prelude          as Pr

import           Ledger           (CurrencySymbol, PubKeyHash, TokenName,
                                   ValidatorHash)
import           Plutus.Contract  (Endpoint, type (.\/))
import qualified PlutusTx
import           PlutusTx.Prelude as Plutus (Eq (..), Integer, (&&))
import           Schema           (ToSchema)

data NFTSale = NFTSale
    { nSeller   :: !PubKeyHash
    , nPrice    :: !Plutus.Integer
    , nCurrency :: !CurrencySymbol
    , nToken    :: !TokenName
    } deriving (Generic, ToJSON, FromJSON)

instance Eq NFTSale where
    {-# INLINABLE (==) #-}
    a == b = (nSeller    a == nSeller    b) &&
             (nPrice     a == nPrice     b) &&
             (nCurrency  a == nCurrency  b) &&
             (nToken     a == nToken     b)

PlutusTx.makeIsDataIndexed ''NFTSale [('NFTSale, 0)]
PlutusTx.makeLift ''NFTSale


data SaleAction = Buy | Close
    deriving Show

PlutusTx.makeIsDataIndexed ''SaleAction [('Buy, 0), ('Close, 1)]
PlutusTx.makeLift ''SaleAction


-- We define two different params for the two endpoints start and buy with the minimal info needed.
-- Therefore the user doesn't have to provide more that what's needed to execute the said action.
{-

   For StartParams we ommit the seller
    because we automatically input the address of the wallet running the startSale enpoint

   For BuyParams we ommit seller and price
    because we can read that in datum which can be obtained with just cs and tn of the sold token

-}

data BuyParams = BuyParams
    { bCs :: CurrencySymbol
    , bTn :: TokenName
    } deriving (Pr.Eq, Pr.Ord, Show, Generic, ToJSON, FromJSON, ToSchema)

data StartParams = StartParams
    { sPrice :: Integer
    , sCs    :: CurrencySymbol
    , sTn    :: TokenName
    } deriving (Pr.Eq, Pr.Ord, Show, Generic, ToJSON, FromJSON, ToSchema)


type SaleSchema =
    Endpoint "start" StartParams
    .\/
    Endpoint "buy" BuyParams
    .\/
    Endpoint "close" BuyParams

