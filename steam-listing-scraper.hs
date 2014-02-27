import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.SSem
import Control.Monad
import Data.Aeson
import Data.Char
import Data.Either.Utils
import Data.Int
import Data.List
import Data.List.Split
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process
import Text.HTML.TagSoup
import Text.Printf
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as B.L
import qualified Data.ConfigFile as C
import qualified Data.Map as Map
import qualified Data.Text as T

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
    , quantity :: Int
    , price :: Int
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

main :: IO ()
main = do
    -- First argument is maximum number of threads
    args <- getArgs
    let maxThreads = read $ head args :: Int

    -- Create semaphore with maximum amount of threads specified
    sem <- new maxThreads

    -- Read currency rates
    ratesFile <- B.L.readFile "rates.json"
    let maybeRates = fmap decode $ B.L.lines ratesFile :: [Maybe CurrencyRate]
    -- print maybeRates

    let rates = fmap (\(Just x) -> x)  maybeRates
    print rates

    -- Read proxies files
    proxies <- liftM cycle $ liftM lines $ readFile "proxies.txt"

    -- PostgreSQL database connection
    connInfo <- getConnectInfo
    conn <- connect connInfo

    -- Download all listings
    urlsToScrape <- getUrlsToScrape conn
    print $ length urlsToScrape

    let urls = generateCommands urlsToScrape proxies

    status <- newChan
    -- Seperate thread checks directory for new files
    forkIO $ forever $ do
        files <- getDirectoryContents "csgo-listings/"
        let pages = filter (isSuffixOf ".html") files

        if pages == []
            then do
                writeChan status "done"
                threadDelay 1000000
            else do
                writeChan status "busy"
                forM_ (fmap (\x -> "csgo-listings/" ++ x) pages) $
                    \x -> do
                        listings <- readListingsPage rates x
                        storeListings conn listings

    mapM_ forkOS $ map (scrapeListing sem) urls

    forever $ do
        freeThreads <- getValue sem
        if freeThreads /= maxThreads
            then do
                threadDelay 5000000
                return ()
            else do
                message <- readChan status
                if message == "done"
                    then do
                        close conn
                        exitSuccess
                    else do
                        return ()

getUrlsToScrape :: Connection -> IO [String]
getUrlsToScrape conn = do
    let select = Query $ B.pack "SELECT DISTINCT m.url FROM market AS \
        \m LEFT OUTER JOIN listing_history AS l ON m.url = l.url \
        \WHERE l.id IS null OR m.url IN (SELECT DISTINCT url FROM \
        \listing_history WHERE item_price > 10) OR m.url IN \
        \(SELECT l.url FROM listing_history as l, market as m WHERE \
        \l.url = m.url AND (l.item_price < 10) AND \
        \(m.price > (l.item_price + 5)))"
    urls <- query_ conn select
    let urlStrings = fmap (\(Only url) -> B.unpack url) urls
    return urlStrings

-- Generate commands to run
generateCommands :: [String] -> [String] -> [String]
generateCommands [] _ = []
generateCommands (x:[]) (proxy:proxies) = ("casperjs --proxy=" ++ ip ++ " \
    \--proxy-auth=" ++ auth ++ " market-page.js '" ++ x
        ++ "' csgo-listings/") : []
    where splitProxies = splitOn ":" proxy
          ip = (splitProxies !! 0) ++ ":" ++ (splitProxies !! 1)
          auth = (splitProxies !! 2) ++ ":" ++ (splitProxies !! 3)
generateCommands (x:xs) (proxy:proxies) = ("casperjs --proxy=" ++ ip ++ " \
    \--proxy-auth=" ++ auth ++ " market-page.js '" ++ x
        ++ "' csgo-listings/") : generateCommands xs proxies
    where splitProxies = splitOn ":" proxy
          ip = (splitProxies !! 0) ++ ":" ++ (splitProxies !! 1)
          auth = (splitProxies !! 2) ++ ":" ++ (splitProxies !! 3)

-- Download a steam listing page
scrapeListing :: SSem -> String -> IO ()
scrapeListing sem url = do
    wait sem
    print $ "Starting " ++ url
    handle <- runCommand url
    waitForProcess handle
    print $ "Finshed " ++ url
    signal sem
    return ()

readListingsPage :: [CurrencyRate] -> FilePath ->
    IO (Maybe [ItemListing], Maybe ItemListing)
readListingsPage rates path = do
    file <- readFile path
    removeFile path
    return $ scrapeListingsPage rates file

storeListings :: Connection -> (Maybe [ItemListing], Maybe ItemListing)
    -> IO Int64
storeListings _ (_, Nothing) = return 0
-- Checks if there are any underpriced rows and deletes them because
-- no new underpriced were found
storeListings conn (Nothing, Just listing) = do
    let select = Query $ B.pack $ "SELECT listing_no, url, item_price, \
        \item_price_before_fee FROM underpriced WHERE url = \
        \'" ++ itemUrl listing ++ "'"
    existing <- query_ conn select :: IO [ItemListing]
    if length existing == 0
        then do
            insertListing conn listing
            return 1
        else do
            deleteUnderpriced conn $ itemUrl listing
            insertListing conn listing
            return 1
storeListings conn (Just under, Just listing) = do
    -- Get id of inserted listing to have a reference when checking profit
    listingId <- insertListing conn listing
    let listingId' = fromOnly $ head listingId
    let select = Query $ B.pack $ "SELECT listing_no, url, item_price, \
        \item_price_before_fee FROM underpriced WHERE url = \
        \'" ++ itemUrl listing ++ "'"
    existing <- query_ conn select :: IO [ItemListing]
    if length existing == 0
        then do
            liftM sum $ forM under $ insertUnderpriced conn listingId'
        else do
            deleteUnderpriced conn $ itemUrl listing
            liftM sum $ forM under $ insertUnderpriced conn listingId'

insertListing :: Connection -> ItemListing -> IO [Only Int]
insertListing conn item = query conn insert item
    where insert = Query $ B.pack "INSERT INTO listing_history (listing_no, \
        \url, item_price, item_price_before_fee) VALUES (?,?,?,?) \
        \RETURNING id"

insertUnderpriced :: Connection -> Int -> ItemListing -> IO Int64
insertUnderpriced conn listingId item = do
    let select = Query $ B.pack $ "SELECT listing_no, url, item_price, \
        \item_price_before_fee FROM underpriced WHERE listing_no = \
        \'" ++ listingNo item ++ "'"
    existing <- query_ conn select :: IO [ItemListing]
    if length existing == 0
        then do
            let insert = Query $ B.pack $ "INSERT INTO underpriced \
                \(listing_id, listing_no, url, item_price, \
                \item_price_before_fee) VALUES \
                \(" ++ (show listingId) ++ ",?,?,?,?)"
            execute conn insert item
        else do
            return 0

deleteUnderpriced :: Connection -> String -> IO Int64
deleteUnderpriced conn url = do
    let delete = Query $ B.pack $ "DELETE FROM underpriced WHERE url = \
        \'" ++ url ++ "'"
    execute_ conn delete

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
          formatted = f $
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
    case fst price of (-1) -> Nothing
                      _ -> Just (ItemListing listingNo itemUrl
                            (fst price) (snd price))
    where listingNo = scrapeListingNo listing
          price = scrapePrice rates listing

scrapeListingNo :: [Tag String] -> String
scrapeListingNo listing = filter isDigit $ getAttrib "id" listingNo
    where listingNo = head . sections (~== "<div>") $ listing

-- Edge cases: "Sold!", "12,--"
scrapePrice :: [CurrencyRate] -> [Tag String] -> (Int, Int)
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
trim ('-':xs) = '0' : trim xs
trim (x:xs) = x : trim xs

-- Convert the original listing currency to USD
-- Returns "-1" if Sold! or unknown currency
-- If using US proxy USD is not shown
convert :: [CurrencyRate] -> String -> Int
convert rates price
    | rub `isInfixOf` price = exchange rates "RUB" $ removeCurrency price rub
    | gbp `isInfixOf` price = exchange rates "GBP" $ removeCurrency price gbp
    | brl `isInfixOf` price = exchange rates "BRL" $ removeCurrency price brl
    | eur `isInfixOf` price = exchange rates "EUR" $ removeCurrency price eur
    | "USD" `isInfixOf` price = truncate $ (*) 100 $ read $ 
        filter (\x -> isDigit x || '.' == x) price
    | "$" `isInfixOf` price = truncate $ (*) 100 $ read $
        filter (\x -> isDigit x || '.' == x) price
    | otherwise = (-1)
    where rub = eliminate $ Map.lookup "RUB" currencyMap
          gbp = eliminate $ Map.lookup "GBP" currencyMap
          brl = eliminate $ Map.lookup "BRL" currencyMap
          eur = eliminate $ Map.lookup "EUR" currencyMap

eliminate :: Maybe [a] -> [a]
eliminate maybeValue = case maybeValue of (Just x) -> x
                                          Nothing -> []

exchange :: [CurrencyRate] -> String -> String -> Int
-- Multiply final result by 100 because we are storing value as int
exchange rates cur price = truncate $ (*) 100 $ 
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

getConnectInfo :: IO ConnectInfo
getConnectInfo = do
    val <- C.readfile C.emptyCP "sms.conf"
    let cp = forceEither val
    let database = forceEither $ C.get cp "DEFAULT" "database"
    let db_host = forceEither $ C.get cp "DEFAULT" "db_host"
    let db_port = forceEither $ C.get cp "DEFAULT" "db_port"
    let db_user = forceEither $ C.get cp "DEFAULT" "db_user"
    let db_pass = forceEither $ C.get cp "DEFAULT" "db_pass"
    return $ ConnectInfo db_host db_port db_user db_pass database
