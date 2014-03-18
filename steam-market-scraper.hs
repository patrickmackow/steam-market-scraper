import Control.Concurrent
import Control.Concurrent.SSem
import Control.Monad
import Data.Char
import Data.Int
import Data.List
import Data.List.Split
import System.Environment
import System.Exit
import System.IO
import System.Process
import System.Random

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

    -- Read proxy list
    timeout <- liftM last $ liftM lines $ readFile "proxies.txt"
    proxies <- liftM cycle $ liftM init $ liftM lines $ readFile "proxies.txt"

    -- Download all market pages
    total <- readProcess "casperjs" ["market-total.js"
        ,"http://steamcommunity.com/market/search?q=appid%3A730", timeout] []
    -- Exit with failure if steam market is down
    -- TODO: Email notification
    print total

    if total == "null\n"
        then exitFailure
        else do
            let urls = generateCommands (read . head . lines $ total)
                    proxies timeout

            mapM_ forkOS $ map (scrapeMarket sem) urls

            forever $ do
                freeThreads <- getValue sem
                if freeThreads /= maxThreads
                    then do
                        threadDelay 5000000
                    else do
                        exitSuccess

-- Generate commands to run
generateCommands :: Int -> [String] -> String -> [String]
generateCommands 1 (proxy:proxies) timeout = ("casperjs --proxy=" ++ ip ++ " \
    \--proxy-auth=" ++ auth ++ " market-page.js \
    \'http://steamcommunity.com/market/search?q=appid%3A730#p"
        ++ (show 1) ++ "' " ++ timeout ++ " csgo-pages/") : []
    where splitProxies = splitOn ":" proxy
          ip = (splitProxies !! 0) ++ ":" ++ (splitProxies !! 1)
          auth = (splitProxies !! 2) ++ ":" ++ (splitProxies !! 3)
generateCommands x (proxy:proxies) timeout = ("casperjs --proxy=" ++ ip ++ " \
    \--proxy-auth=" ++ auth ++ " market-page.js \
    \'http://steamcommunity.com/market/search?q=appid%3A730#p"
        ++ (show x) ++ "' " ++ timeout ++ " csgo-pages/")
        : generateCommands (x - 1) proxies timeout
    where splitProxies = splitOn ":" proxy
          ip = (splitProxies !! 0) ++ ":" ++ (splitProxies !! 1)
          auth = (splitProxies !! 2) ++ ":" ++ (splitProxies !! 3)

scrapeMarket :: SSem -> String -> IO ()
scrapeMarket sem url = do
    wait sem
    time <- liftM (*1000) $ randomRIO(100, 250) :: IO Int
    threadDelay time
    print $ show time ++ ": Starting " ++ url
    handle <- runCommand url
    waitForProcess handle
    print $ "Finished " ++ url
    signal sem
    return ()
