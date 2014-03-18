import Control.Concurrent
import Control.Concurrent.SSem
import Control.Monad
import Data.Int
import Data.List
import Data.List.Split
import Data.Time
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types
import System.Environment
import System.Exit
import System.IO
import System.Process
import System.Random
import Utils
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = do
    -- First argument is maximum number of threads
    args <- getArgs
    if (length args == 0)
        then exitFailure -- Exit if no arguments found
        else return ()

    let maxThreads = read $ head args :: Int

    -- Create semaphore with maximum amount of threads specified
    sem <- new maxThreads

    -- Read proxies files
    timeout <- liftM last $ liftM lines $ readFile "proxies.txt"
    proxies <- liftM cycle $ liftM init $ liftM lines $ readFile "proxies.txt"

    -- PostgreSQL database connection
    connInfo <- getConnectInfo
    conn <- connect connInfo

    -- Download all listings
    urlsToScrape <- getUrlsToScrape conn
    print $ length urlsToScrape

    insertLastRun conn
    deleteAllUnderpriced conn

    let urls = generateCommands urlsToScrape proxies timeout

    mapM_ forkOS $ map (scrapeListing sem) urls

    forever $ do
        freeThreads <- getValue sem
        if freeThreads /= maxThreads
            then do
                threadDelay 5000000
            else do
                close conn
                exitSuccess

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
generateCommands :: [String] -> [String] -> String -> [String]
generateCommands [] _ _ = []
generateCommands (x:[]) (proxy:proxies) timeout = ("casperjs --proxy="
        ++ ip ++ " --proxy-auth=" ++ auth ++ " market-page.js '" ++ x
        ++ "' " ++ timeout ++ " csgo-listings/") : []
    where splitProxies = splitOn ":" proxy
          ip = (splitProxies !! 0) ++ ":" ++ (splitProxies !! 1)
          auth = (splitProxies !! 2) ++ ":" ++ (splitProxies !! 3)
generateCommands (x:xs) (proxy:proxies) timeout = ("casperjs --proxy="
        ++ ip ++ " --proxy-auth=" ++ auth ++ " market-page.js '" ++ x
        ++ "' " ++ timeout ++ " csgo-listings/")
        : generateCommands xs proxies timeout
    where splitProxies = splitOn ":" proxy
          ip = (splitProxies !! 0) ++ ":" ++ (splitProxies !! 1)
          auth = (splitProxies !! 2) ++ ":" ++ (splitProxies !! 3)

-- Download a steam listing page
scrapeListing :: SSem -> String -> IO ()
scrapeListing sem url = do
    wait sem
    time <- liftM (*1000) $ randomRIO(100, 250) :: IO Int
    threadDelay time
    print $ show time ++ ": Starting " ++ url
    handle <- runCommand url
    waitForProcess handle
    print $ "Finshed " ++ url
    signal sem
    return ()

insertLastRun :: Connection -> IO Int64
insertLastRun conn = do
    datetime <- getCurrentTime
    let insert = Query $ B.pack $ "INSERT INTO last_run (latest_timestamp) \
        \VALUES ('" ++ (show datetime) ++ "')"
    execute_ conn insert

deleteAllUnderpriced :: Connection -> IO Int64
deleteAllUnderpriced conn = do
    let delete = Query $ B.pack $ "DELETE FROM underpriced"
    execute_ conn delete
