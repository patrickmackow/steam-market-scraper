import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Char
import Data.Int
import Data.List
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import MarketTypes
import MarketTypes
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.Process
import Text.HTML.TagSoup
import Text.HTML.TagSoup
import Utils
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as B.L
import qualified Data.Map as Map
import qualified Data.Text as T

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

main :: IO ()
main = do
    -- Arguments will be file-scraper [test] market|listing file
    -- test will output to stdout
    args <- getArgs
    parseArgs args

parseArgs :: [String] -> IO ()
parseArgs [] = exitFailure
parseArgs (pageType:path:[])
    | pageType == "market" = do
        connInfo <- getConnectInfo
        conn <- connect connInfo

        -- Get items using readMarketPage function
        items <- readMarketPage path
        storeItems conn items
        return ()

    | pageType == "listing" = do
        ratesFile <- B.L.readFile "rates.json"
        let maybeRates = fmap decode $
                B.L.lines ratesFile :: [Maybe CurrencyRate]
            rates = fmap (\(Just x) -> x)  maybeRates

        connInfo <- getConnectInfo
        conn <- connect connInfo

        -- Get listings using readListingsPage function
        listings <- readListingsPage rates path
        storeListings conn listings
        return ()

    | otherwise = exitFailure
-- Test argument found
parseArgs (test:pageType:path:[]) = do
    undefined

readMarketPage :: FilePath -> IO [MarketItem]
readMarketPage path = do
    file <- readFile path
    return $ scrapeMarketPage file

storeItems :: Connection -> [MarketItem] -> IO Int64
storeItems conn [] = return 0
storeItems conn (item:[]) = do
    let select = Query $ B.pack $ "SELECT url, image, quantity, price, \
        \item_name, item_name_colour, game FROM market WHERE \
        \url = '" ++ url item ++ "'"
    exists <- query_ conn select :: IO [MarketItem]
    if length exists == 0
        then insertItem conn item
        else updateItem conn
            (UpdatedMarketItem (quantity item) (price item) (url item))
storeItems conn (item:items) = do
    let select = Query $ B.pack $ "SELECT url, image, quantity, price, \
        \item_name, item_name_colour, game FROM market WHERE \
        \url = '" ++ url item ++ "'"
    exists <- query_ conn select :: IO [MarketItem]
    if length exists == 0
        then insertItem conn item
        else updateItem conn
            (UpdatedMarketItem (quantity item) (price item) (url item))
    storeItems conn items

insertItem :: Connection -> MarketItem -> IO Int64
insertItem conn item = execute conn insert item
    where insert = Query $ B.pack "INSERT INTO market (url, image, \
             \quantity, price, item_name, item_name_colour, game) \
             \VALUES (?,?,?,?,?,?,?)"

updateItem :: Connection -> UpdatedMarketItem -> IO Int64
updateItem conn item = execute conn update item
    where update = Query $ B.pack "UPDATE market SET quantity = ?, \
             \price = ? WHERE url = ?"

-- Scrape info from market page
scrapeMarketPage :: String -> [MarketItem]
scrapeMarketPage page = fmap scrapeMarketItem items
    where items = sections (~== "<a class=market_listing_row_link>")
                  $ parseTags page

scrapeMarketItem :: [Tag String] -> MarketItem
scrapeMarketItem item =
    MarketItem url image quantity price name nameColour game
    where url = scrapeUrl item
          image = scrapeImage item
          quantity = scrapeQuantity item
          price = scrapeMarketPrice item
          name = scrapeName item
          nameColour = scrapeNameColour item
          game = scrapeGame item

scrapeUrl :: [Tag String] -> String
scrapeUrl item = getAttrib "href" item

scrapeImage :: [Tag String] -> String
scrapeImage item = getAttrib "src" image
    where image = head . sections (~== "<img>") $ item

scrapeQuantity :: [Tag String] -> Int
scrapeQuantity item = read . filter (/= ',') $ getTagText quantity
    where quantity = head . sections (~== "<span\
        \ class=market_listing_num_listings_qty>") $ item

scrapeMarketPrice :: [Tag String] -> Int
scrapeMarketPrice item = truncate $ (*) 100 $ read . g . fromTagText . head $ priceText
    where priceTag = takeWhile (~/= "</span>") $ dropWhile (~/= "<br>") item
          priceText = filter f $ filter isTagText priceTag
          -- Find Tag which contains price
          f :: Tag String -> Bool
          f x = '$' `elem` fromTagText x
          -- Get price only
          g :: String -> String
          g x = drop 1 . takeWhile (/= ' ') $ dropWhile (/= '$') x

scrapeName :: [Tag String] -> String
scrapeName item = getTagText name
    where name = head . sections (~== "<span\
        \ class=market_listing_item_name>") $ item

scrapeNameColour :: [Tag String] -> String
scrapeNameColour item = f $ getAttrib "style" colour
    where colour = head . sections (~== "<span\
        \ class=market_listing_item_name>") $ item
          -- Get CSS colour
          f :: String -> String
          f x = takeWhile (/= ';') $ dropWhile (/= '#') x

scrapeGame :: [Tag String] -> String
scrapeGame item = getTagText game
    where game = head . sections (~== "<span\
        \ class=market_listing_game_name>") $ item

readListingsPage :: [CurrencyRate] -> FilePath ->
    IO (Maybe [ItemListing], Maybe ItemListing)
readListingsPage rates path = do
    file <- readFile path
    -- Check for empty file
    if (length $ lines file) == 0
        then return (Nothing, Nothing)
        else return $ scrapeListingsPage rates file

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
scrapeItemListing _ _ [] = Nothing
scrapeItemListing rates itemUrl listing =
    case fst price of (-1) -> Nothing
                      _ -> Just (ItemListing listingNo itemUrl
                            (fst price) (snd price))
    where listingNo = scrapeListingNo listing
          price = scrapeListingPrice rates listing

scrapeListingNo :: [Tag String] -> String
scrapeListingNo [] = ""
scrapeListingNo listing = filter isDigit $ getAttrib "id" listingNo
    where listingNo = head . sections (~== "<div>") $ listing

-- Edge cases: "Sold!", "12,--"
scrapeListingPrice :: [CurrencyRate] -> [Tag String] -> (Int, Int)
scrapeListingPrice _ [] = ((-1), (-1))
scrapeListingPrice rates listing = (convert rates . trim $ getTagText price,
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
getAttrib _ [] = ""
getAttrib atr tag = fromAttrib atr . head . filter isTagOpen $ tag

getTagText :: [Tag String] -> String
getTagText [] = ""
getTagText tag = fromTagText . head . filter isTagText $ tag

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
convert _ [] = (-1)
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
