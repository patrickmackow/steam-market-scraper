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
import qualified Data.ByteString.Lazy.Char8 as B.L
import qualified Data.Text as T
import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.Map as Map
import Text.Printf

import Paths_steam_market_scraper

currencies = [ ("RUB", "p\1091\1073.")
             , ("GBP", "\163\&")
             , ("BRL", "R$")
             , ("EUR", "\8364")]
currencyMap = Map.fromList currencies

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
    -- Read currency rates
    ratesFile <- B.L.readFile "rates.json"
    let maybeRates = fmap decode $ B.L.lines ratesFile :: [Maybe CurrencyRate]
    print maybeRates
    
    let rates = fmap (\(Just x) -> x)  maybeRates
    print rates

    -- PostgreSQL database connection
    -- conn <- connect defaultConnectInfo { connectUser = "patrick"
    --                                    , connectPassword = "gecko787"
    --                                    , connectDatabase = "steam_market" }
    -- Download all listings
    -- urlsToScrape <- getUrlsToScrape conn
    -- print $ length urlsToScrape
    -- scrapeListings urlsToScrape

    files <- getDirectoryContents "csgo-listings/"
    let filteredFiles = filter (isSuffixOf ".html") files

    print filteredFiles

    listings <- mapM (\x -> readListingsPage rates x)
        $ fmap (\x -> "csgo-listings/" ++ x) filteredFiles

    print listings

    -- Close database connection
    -- close conn

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

readListingsPage :: [CurrencyRate] -> FilePath -> IO [ItemListing]
readListingsPage rates path = do
    file <- readFile path
    -- removeFile path
    return $ scrapeListingsPage rates file

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
scrapeListingsPage :: [CurrencyRate] -> String -> [ItemListing]
scrapeListingsPage rates page = fmap (scrapeItemListing rates) listings
    where listings = sections (~== TagOpen "div" [("id",""), ("class","")])
                        $ parseTags page

scrapeItemListing :: [CurrencyRate] -> [Tag String] -> ItemListing
scrapeItemListing rates listing = ItemListing listingNo itemUrl (fst price) (snd price)
    where listingNo = "test"
          itemUrl = "test"
          price = scrapePrice rates listing

scrapeUrl :: [Tag String] -> String
scrapeUrl = undefined
-- scrapeUrl item = getAttrib "href" item

-- Edge cases: "Sold!"
scrapePrice :: [CurrencyRate] -> [Tag String] -> (String, String)
scrapePrice rates listing = (convert rates . trim $ getTagText price, 
    convert rates . trim $ getTagText priceBeforeFee)
    where price = head . sections (~== "<span class='market_listing_price market_\
                    \listing_price_with_fee'>")
                    $ listing
          priceBeforeFee  = head . sections (~== "<span class='market_listing_price market_\
                                \listing_price_without_fee'>")
                                $ listing
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

trim :: String -> String
trim [] = []
trim (' ':xs) = trim xs
trim (',':xs) = '.' : trim xs
trim ('\n':xs) = trim xs
trim ('\t':xs) = trim xs
trim (x:xs) = x : trim xs

-- Convert the original listing currency to USD
-- Returns "-1" if Sold! or unknown currency
convert :: [CurrencyRate] -> String -> String
convert rates price
    | rub `isInfixOf` price = exchange rates "RUB" $ removeCurrency price rub
    | gbp `isInfixOf` price = exchange rates "GBP" $ removeCurrency price gbp
    | brl `isInfixOf` price = exchange rates "BRL" $ removeCurrency price brl
    | eur `isInfixOf` price = exchange rates "EUR" $ removeCurrency price eur
    | "USD" `isInfixOf` price = filter (\x -> isDigit x || '.' == x) price
    | otherwise = "-1"
    where rub = eliminate $ Map.lookup "RUB" currencyMap
          gbp = eliminate $ Map.lookup "GBP" currencyMap
          brl = eliminate $ Map.lookup "BRL" currencyMap
          eur = eliminate $ Map.lookup "EUR" currencyMap

eliminate :: Maybe [a] -> [a]
eliminate maybeValue = case maybeValue of (Just x) -> x
                                          Nothing -> []

exchange :: [CurrencyRate] -> String -> String -> String
exchange rates cur price = printf "%.2f" $ (read price) * (currencyRate rates cur)
    where currencyRate (x:xs) cur
            | currency x == cur = rate x
            | otherwise = currencyRate xs cur

-- Remove the currency symbol from the price
removeCurrency :: String -> String -> String
removeCurrency [] _ = []
removeCurrency price@(x:xs) [] = x : removeCurrency xs []
removeCurrency price@(x:xs) currency@(y:ys)
    | x == y = removeCurrency xs ys
    | otherwise = x : removeCurrency xs currency
