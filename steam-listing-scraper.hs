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

instance Ord ItemListing where
    compare (ItemListing _ _ a _) (ItemListing _ _ b _) = compare a b

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

readListingsPage :: [CurrencyRate] -> FilePath ->
    IO (Maybe [ItemListing], Maybe ItemListing)
readListingsPage rates path = do
    file <- readFile path
    -- removeFile path
    return $ scrapeListingsPage rates file

storeListings :: Connection -> (Maybe [ItemListing], Maybe ItemListing)
    -> IO Int64
storeListing _ (_, Nothing) = return 0
storeListing conn (Nothing, Just listing) = do
    insertListing conn listing
    return 1
storeListings conn (Just under, Just listing) = do
    -- Get id of inserted listing to have a reference when checking profit
    listingId <- insertListing conn listing
    let listingId' = fromOnly $ head listingId
    liftM sum $ forM under $ insertUnderpriced conn listingId'
-- storeListings conn items = do
--     let select = Query $ B.pack "SELECT url FROM market"
--     existing <- query_ conn select
--     let existingStrings = fmap (\(Only url) -> B.unpack url) existing
--     -- forM_ existing $ \(Only url) -> print $ B.unpack url
--     print $ length existing
--     forM_ items $ \item -> checkItem conn existingStrings item
--     return ()

insertListing :: Connection -> ItemListing -> IO [Only Int]
insertListing conn item = query conn insert item
    where insert = Query $ B.pack "INSERT INTO listing (listing_no, url, \
            \item_price, item_price_before_fee) VALUES (?,?,?,?) RETURNING id"

insertUnderpriced :: Connection -> Int -> ItemListing -> IO Int64
insertUnderpriced conn listingId item = do
    let select = Query $ B.pack "SELECT listing_no, url, item_price, \
        \item_price_before_fee FROM underpriced WHERE listing_no = \
        \'" ++ listingNo item ++ "'"
    existing <- query_ conn select :: IO [ItemListing]
    if length existing == 0
        then do
            let insert = Query $ B.pack "INSERT INTO underpriced \
                \(listing_id, listing_no, url, item_price, item_price_before_fee) \
                \VALUES ('" ++ listingId ++ "' ?,?,?,?)"
        else do
            return 0


-- Scrape info from market page
-- Returns a pair: left side is any underpriced ItemListing
--                 right side is a normal priced ItemListing
scrapeListingsPage :: [CurrencyRate] -> String ->
    (Maybe [ItemListing], Maybe ItemListing)
scrapeListingsPage rates page = findUnderpriced formatted
    -- First line of file of listing url
    where itemUrl = head . take 1 $ lines page
          listings = sections (~== TagOpen "div" [("id",""), ("class","")])
                        $ parseTags . unlines . drop 1 . lines $ page
          formatted = sort . f $
            fmap (scrapeItemListing rates itemUrl) listings
          f xs = foldr g [] xs
          g x xs = case x of Nothing -> xs
                             Just x' -> (x':xs)

findUnderpriced :: [ItemListing] ->
    (Maybe [ItemListing], Maybe ItemListing)
findUnderpriced [] = (Nothing, Nothing)
findUnderpriced (x:[]) = (Nothing, Just x)
findUnderpriced items = case under of Nothing -> (under, Just $ head items)
                                      _ -> (under, Just $ head $
                                            drop underLength items)
    where under = f items []
          underLength = case under of Nothing -> 0
                                      Just xs -> length xs
          f :: [ItemListing] -> [ItemListing] -> Maybe [ItemListing]
          f [] _ = Nothing
          f (x:[]) _ = Nothing
          f (x:xs) []
            | itemPrice x < priceBeforeFee (head xs) = Just (x:[])
            | otherwise = f xs (x:[])
          f (x:xs) under
            | itemPrice x < priceBeforeFee (head xs) = Just (x:under)
            | otherwise = f xs (x:under)

-- If listing is sold or has unknown currency return Nothing
scrapeItemListing :: [CurrencyRate] -> String -> [Tag String] ->
    Maybe ItemListing
scrapeItemListing rates itemUrl listing =
    case fst price of "-1" -> Nothing
                      _ -> Just (ItemListing listingNo itemUrl
                            (fst price) (snd price))
    where listingNo = scrapeListingNo listing
          price = scrapePrice rates listing

scrapeListingNo :: [Tag String] -> String
scrapeListingNo listing = filter isDigit $ getAttrib "id" listingNo
    where listingNo = head . sections (~== "<div>") $ listing

-- Edge cases: "Sold!"
scrapePrice :: [CurrencyRate] -> [Tag String] -> (String, String)
scrapePrice rates listing = (convert rates . trim $ getTagText price,
    convert rates . trim $ getTagText priceBeforeFee)
    where price = head . sections (~== "<span class=\
                    \'market_listing_price market_\
                    \listing_price_with_fee'>")
                    $ listing
          priceBeforeFee  = head . sections (~== "<span class=\
                                \'market_listing_price market_\
                                \listing_price_without_fee'>")
                                $ listing

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
exchange rates cur price = printf "%.2f" $
    (read price) * (currencyRate rates cur)
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
