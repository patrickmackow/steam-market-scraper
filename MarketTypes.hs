module MarketTypes where

import Control.Applicative
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow

-- Items that will be scraped from market page
data MarketItem = MarketItem
    { url :: String
    , image :: String
    , quantity :: Int
    , price :: Int
    , name :: String
    , nameColour :: String
    , game :: String
    } deriving (Show)

instance Eq MarketItem where
    x == y = url x == url y

instance ToRow MarketItem where
    toRow m = [toField (url m), toField (image m), toField (quantity m),
        toField (price m), toField (name m), toField (nameColour m),
        toField (game m)]

instance FromRow MarketItem where
    fromRow = MarketItem <$> field <*> field <*> field
        <*> field <*> field <*> field <*> field

-- UpdatedMarketItem needs to be in this order for update statement
data UpdatedMarketItem = UpdatedMarketItem
    { quantity' :: Int
    , price' :: Int
    , url' :: String
    }

instance ToRow UpdatedMarketItem where
    toRow m = [toField (quantity' m), toField (price' m), toField (url' m)]

-- Items that will be scraped from listings page
data ItemListing = ItemListing
    { listingNo :: String
    , itemUrl :: String
    , itemPrice :: Int
    , priceBeforeFee :: Int
    } deriving (Show, Eq)

-- String numeric values are not the same as double numeric values
instance Ord ItemListing where
    compare (ItemListing _ _ a _) (ItemListing _ _ b _) = compare a b

instance ToRow ItemListing where
    toRow i = [toField (listingNo i), toField (itemUrl i),
        toField (itemPrice i), toField (priceBeforeFee i)]

instance FromRow ItemListing where
    fromRow = ItemListing <$> field <*> field <*> field <*> field
