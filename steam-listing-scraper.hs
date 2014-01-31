import Text.HTML.TagSoup
import System.IO
import System.Process
import System.Directory
import Data.Char
import Data.List
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types
import Data.Int
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import Control.Applicative
import Control.Monad
import Data.Aeson

import Paths_steam_market_scraper

currencies = [ ("RUB", "p\1091\1073.")
             , ("GBP", "\163\&")
             , ("BRL", "R$")
             , ("EUR", "\8364")]

data CurrencyRate = CurrencyRate
    { currency :: String
    , rate :: Double
    } deriving (Show)

instance FromJSON CurrencyRate where
    parseJSON (Object v) = CurrencyRate <$>
                           v .: T.pack "currency" <*>
                           v .: T.pack "rate"

data MarketItem = MarketItem 
    { url :: String
    , image :: String
    , quantity :: String
    , price :: String
    , name :: String
    , nameColour :: String
    , game :: String
    } deriving (Show, Eq)

instance ToRow MarketItem where
    toRow m = [toField (url m), toField (image m), toField (quantity m), 
        toField (price m), toField (name m), toField (nameColour m), 
        toField (game m)]

instance FromRow MarketItem where
    fromRow = MarketItem <$> field <*> field <*> field 
        <*> field <*> field <*> field <*> field

data ItemListing = ItemListing 
    { listingNo :: String
    , itemUrl :: String
    , itemPrice :: String
    , priceBeforeFee :: String
    } deriving (Show, Eq)

instance ToRow ItemListing where
    toRow i = [toField (listingNo i), toField (itemUrl i), 
        toField (itemPrice i), toField (priceBeforeFee i)]

instance FromRow ItemListing where
    fromRow = ItemListing <$> field <*> field <*> field <*> field

main :: IO ()
main = do
    -- PostgreSQL database connection
    conn <- connect defaultConnectInfo { connectUser = "patrick"
                                       , connectPassword = "gecko787"
                                       , connectDatabase = "steam_market" }
    -- Download all listings
    urlsToScrape <- getUrlsToScrape conn
    print $ length urlsToScrape
    -- scrapeListings urlsToScrape

    files <- getDirectoryContents "csgo-listings/"
    let filteredFiles = filter (isSuffixOf ".html") files

    print filteredFiles

    listings <- mapM (\x -> readListingsPage x)
        $ fmap (\x -> "csgo-listings/" ++ x) filteredFiles

    print listings

    -- Close database connection
    close conn

getUrlsToScrape :: Connection -> IO [String]
getUrlsToScrape conn = do
    let select = Query $ B.pack "SELECT url FROM market"
    urls <- query_ conn select
    let urlStrings = fmap (\(Only url) -> B.unpack url) urls
    return urlStrings

-- Download all Steam Market pages
scrapeListings :: [String] -> IO ()
scrapeListings (url:[]) = do
    handle <- runCommand $ ("casperjs market-page.js\
        \ '" ++ url ++ "' csgo-listings/")
    waitForProcess handle
    return ()
scrapeListings (url:urls) = do
    handle <- runCommand $ ("casperjs market-page.js\
        \ '" ++ url ++ "' csgo-listings/")
    waitForProcess handle
    scrapeListings urls

readListingsPage :: FilePath -> IO [ItemListing]
readListingsPage path = do
    file <- readFile path
    -- removeFile path
    return $ scrapeListingsPage file

storeListings :: Connection -> [MarketItem] -> IO ()
storeListings = undefined
-- storeListings conn items = do
--     let select = Query $ B.pack "SELECT url FROM market"
--     existing <- query_ conn select
--     let existingStrings = fmap (\(Only url) -> B.unpack url) existing
--     -- forM_ existing $ \(Only url) -> print $ B.unpack url
--     print $ length existing
--     forM_ items $ \item -> checkItem conn existingStrings item
--     return ()

insertListings :: Connection -> MarketItem -> IO Int64
insertListings = undefined
-- insertListings conn item = execute conn insert item
--     where insert = Query $ B.pack "INSERT INTO market (url, image, \
--              \quantity, price, item_name, item_name_colour, game) \
--              \VALUES (?,?,?,?,?,?,?)"

-- Scrape info from market page
scrapeListingsPage :: String -> [ItemListing]
scrapeListingsPage page = fmap scrapeItemListing listings
    where listings = sections (~== TagOpen "div" [("id",""), ("class","")])
                        $ parseTags page

scrapeItemListing :: [Tag String] -> ItemListing
scrapeItemListing listing = ItemListing listingNo itemUrl (fst price) (snd price)
    where listingNo = "test"
          itemUrl = "test"
          price = scrapePrice listing

scrapeUrl :: [Tag String] -> String
scrapeUrl = undefined
-- scrapeUrl item = getAttrib "href" item

-- Edge cases: "Sold!"
scrapePrice :: [Tag String] -> (String, String)
scrapePrice listing = (trim $ getTagText price, trim $ getTagText priceBeforeFee)
    where price = head . sections (~== "<span class='market_listing_price market_\
                    \listing_price_with_fee'>")
                    $ listing
          priceBeforeFee  = head . sections (~== "<span class='market_listing_price market_\
                                \listing_price_without_fee'>")
                                $ listing
          trim [] = []
          trim (' ':xs) = trim xs
          trim ('\n':xs) = trim xs
          trim ('\t':xs) = trim xs
          trim (x:xs) = x : trim xs
-- scrapePrice item = g . fromTagText . head $ priceText
--     where priceTag = takeWhile (~/= "</span>") $ dropWhile (~/= "<br>") item
--           priceText = filter f $ filter isTagText priceTag
--           -- Find Tag which contains price
--           f :: Tag String -> Bool
--           f x = '$' `elem` fromTagText x
--           -- Get price only
--           g :: String -> String
--           g x = drop 1 . takeWhile (/= ' ') $ dropWhile (/= '$') x

-- Util functions
getAttrib :: String -> [Tag String] -> String
getAttrib atr tag = fromAttrib atr . head . filter isTagOpen $ tag

getTagText :: [Tag String] -> String
getTagText = fromTagText . head . filter isTagText
