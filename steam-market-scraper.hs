import Control.Applicative
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.SSem
import Control.Exception
import Control.Monad
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
import qualified Data.ByteString.Char8 as B
import qualified Data.ConfigFile as C

import Paths_steam_market_scraper

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

main :: IO ()
main = do
    -- First argument is maximum number of threads
    args <- getArgs
    let maxThreads = read $ head args :: Int

    -- Create semaphore with maximum amount of threads specified
    sem <- new maxThreads

    -- Read config file
    connInfo <- getConnectInfo

    -- Read proxy list
    proxies <- liftM cycle $ liftM lines $ readFile "proxies.txt"

    -- Download all market pages
    total <- readProcess "casperjs" ["market-total.js"
        ,"http://steamcommunity.com/market/search?q=appid%3A730"] []
    -- Exit with failure if steam market is down
    -- TODO: Email notification
    print total

    if total == "null\n"
        then exitFailure
        else do
            let urls = generateCommands (read . head . lines $ total) proxies

            conn <- connect connInfo

            -- Create a chan for the status of the file reader
            status <- newChan
            -- Seperate thread checks directory for new files
            forkIO $ forever $ do
                files <- getDirectoryContents "csgo-pages/"
                let pages = filter (isSuffixOf ".html") files

                if pages == []
                    then do
                        writeChan status "done"
                        threadDelay 1000000
                    else do
                        writeChan status "busy"
                        forM_ (fmap (\x -> "csgo-pages/" ++ x) pages) $
                            \x -> do
                                items <- catch (readMarketPage x) (\e -> do
                                    let err = show (e :: ErrorCall)
                                    print $ "Error: " ++ err
                                    return [])
                                storeItems conn $ nub $ items

            mapM_ forkOS $ map (scrapeMarket sem) urls

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

-- Generate commands to run
generateCommands :: Int -> [String] -> [String]
generateCommands 1 (proxy:proxies) = ("casperjs --proxy=" ++ ip ++ " \
    \--proxy-auth=" ++ auth ++ " market-page.js \
    \'http://steamcommunity.com/market/search?q=appid%3A730#p"
        ++ (show 1) ++ "' csgo-pages/") : []
    where splitProxies = splitOn ":" proxy
          ip = (splitProxies !! 0) ++ ":" ++ (splitProxies !! 1)
          auth = (splitProxies !! 2) ++ ":" ++ (splitProxies !! 3)
generateCommands x (proxy:proxies) = ("casperjs --proxy=" ++ ip ++ " \
    \--proxy-auth=" ++ auth ++ " market-page.js \
    \'http://steamcommunity.com/market/search?q=appid%3A730#p"
        ++ (show x) ++ "' csgo-pages/") : generateCommands (x - 1) proxies
    where splitProxies = splitOn ":" proxy
          ip = (splitProxies !! 0) ++ ":" ++ (splitProxies !! 1)
          auth = (splitProxies !! 2) ++ ":" ++ (splitProxies !! 3)

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
          price = scrapePrice item
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

scrapePrice :: [Tag String] -> Int
scrapePrice item = truncate $ (*) 100 $ read . g . fromTagText . head $ priceText
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
