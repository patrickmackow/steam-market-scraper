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
import Control.Applicative
import Control.Monad
import System.Environment
import Control.Concurrent
import Control.Concurrent.SSem
import System.Exit

import Paths_steam_market_scraper

data MarketItem = MarketItem
    { url :: String
    , image :: String
    , quantity :: String
    , price :: String
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

main :: IO ()
main = do
    -- First argument is maximum number of threads
--     args <- getArgs
--     let maxThreads = read $ head args :: Int
--
--     -- Create semaphore with maximum amount of threads specified
--     sem <- new maxThreads
--
--     -- Download all market pages
--     total <- readProcess "casperjs" ["market-total.js"
--         ,"http://steamcommunity.com/market/search?q=appid%3A730"] []
--     -- Exit with failure if steam market is down
--     -- TODO: Email notification
--     case total of "null\n" -> exitFailure
--     let urls = generateUrls $ read . head . lines $ total
--     print urls
--     mapM_ forkOS $ map (scrapeMarket sem) urls
--     forever $ do
--         freeThreads <- getValue sem
--         if freeThreads /= maxThreads
--             then do
--                 return ()
--             else do
--                 exitSuccess

    files <- getDirectoryContents "csgo-pages/"
    let pages = filter (isSuffixOf ".html") files
    print pages
    items <- mapM (\x -> readMarketPage x)
        $ fmap (\x -> "csgo-pages/" ++ x) pages

    conn <- connect defaultConnectInfo 
                        { connectUser = "patrick"
                        , connectPassword = ""
                        , connectDatabase = "steam_market" }
    storeItems conn $ nub $ concat items
    close conn
    -- test <- readFile "csgo-pages/50"
    -- let items = scrapeMarketPage test
    -- print items
    -- print $ length items

-- Generate URLs to scrape
generateUrls :: Int -> [String]
generateUrls 1 = ("casperjs market-page.js \
    \'http://steamcommunity.com/market/search?q=appid%3A730#p"
        ++ (show 1) ++ "' csgo-pages/") : []
generateUrls x = ("casperjs market-page.js \
    \'http://steamcommunity.com/market/search?q=appid%3A730#p"
        ++ (show x) ++ "' csgo-pages/") : generateUrls (x - 1)

scrapeMarket :: SSem -> String -> IO ()
scrapeMarket sem url = do
    wait sem
    print $ "Starting " ++ url
    handle <- runCommand url
    waitForProcess handle
    print $ "Finished " ++ url
    signal sem
    return ()

readMarketPage :: FilePath -> IO [MarketItem]
readMarketPage path = do
    file <- readFile path
    removeFile path
    return $ scrapeMarketPage file

storeItems :: Connection -> [MarketItem] -> IO Int64
storeItems conn (item:[]) = do
    let select = Query $ B.pack $ "SELECT url, image, quantity, price, \
        \item_name, item_name_colour, game FROM market WHERE \
        \url = '" ++ url item ++ "'"
    exists <- query_ conn select :: IO [MarketItem]
    if length exists == 0
        then insertItem conn item
        else updateItem conn [quantity item, price item, url item]
storeItems conn (item:items) = do
    let select = Query $ B.pack $ "SELECT url, image, quantity, price, \
        \item_name, item_name_colour, game FROM market WHERE \
        \url = '" ++ url item ++ "'"
    exists <- query_ conn select :: IO [MarketItem]
    if length exists == 0
        then insertItem conn item
        else updateItem conn [quantity item, price item, url item]
    storeItems conn items

insertItem :: Connection -> MarketItem -> IO Int64
insertItem conn item = execute conn insert item
    where insert = Query $ B.pack "INSERT INTO market (url, image, \
             \quantity, price, item_name, item_name_colour, game) \
             \VALUES (?,?,?,?,?,?,?)"

updateItem :: Connection -> [String] -> IO Int64
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
          price = scrapePrice item
          name = scrapeName item
          nameColour = scrapeNameColour item
          game = scrapeGame item

scrapeUrl :: [Tag String] -> String
scrapeUrl item = getAttrib "href" item

scrapeImage :: [Tag String] -> String
scrapeImage item = getAttrib "src" image
    where image = head . sections (~== "<img>") $ item

scrapeQuantity :: [Tag String] -> String
scrapeQuantity item = filter (/= ',') $ getTagText quantity
    where quantity = head . sections (~== "<span\
        \ class=market_listing_num_listings_qty>") $ item

scrapePrice :: [Tag String] -> String
scrapePrice item = g . fromTagText . head $ priceText
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

-- Util functions
getAttrib :: String -> [Tag String] -> String
getAttrib atr tag = fromAttrib atr . head . filter isTagOpen $ tag

getTagText :: [Tag String] -> String
getTagText = fromTagText . head . filter isTagText
