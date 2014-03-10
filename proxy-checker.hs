import Control.Concurrent
import Control.Concurrent.SSem
import Control.Concurrent.STM
import Control.Monad
import Data.Either.Utils
import Data.List.Split
import Network.HTTP
import System.Directory
import System.Exit
import System.Process
import qualified Data.ConfigFile as C

testUrl = "http://steamcommunity.com/market/listings/730/Sticker%20Capsule"

main :: IO ()
main = do
    configValues <- liftM forceEither $ C.readfile C.emptyCP
        "proxy-checker.conf"
    let api = forceEither $ C.get configValues "DEFAULT" "api"
    let timeout = splitOn "," $ forceEither $ C.get
            configValues "DEFAULT" "timeout"
    putStrLn api
    print timeout

    removeFile "proxies.txt"
    writeFile "proxies.txt" ""

    -- Get list of proxies from api
    proxyList <- openURL api
    let proxies = splitProxies proxyList

    let len = length proxies
    sem <- new len
    status <- newTMVarIO ([])

    mapM_ forkIO $ map (testProxy sem status timeout) proxies

    threadDelay 1000000

    forever $ do
        freeThreads <- getValue sem
        if freeThreads /= len
            then do
                s <- atomically $ takeTMVar status
                print s
                let split = splitOn "," s
                writeProxy split
                threadDelay 100000
            else do
                exitSuccess

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

testProxy :: SSem -> TMVar String -> [String] -> (String, String) -> IO ()
testProxy sem status timeout proxy@(ip, auth) = do
    wait sem

    let len = length timeout
    sem2 <- new len
    minTime <- newTVarIO (-1)

    mapM_ forkOS $ map (benchmark sem2 minTime proxy) timeout

    forever $ do
        freeThreads <- getValue sem2
        if freeThreads /= len
            then do
                threadDelay 1000000
            else do
                atomically $ do
                    x <- readTVar minTime
                    putTMVar status $ (show x) ++ "," ++ ip ++ ":" ++ auth
                signal sem
                -- Indefinite thread delay so previous statements don't run
                threadDelay 999999999

benchmark :: SSem -> TVar Int -> (String, String) -> String -> IO ()
benchmark sem minTime (ip, auth) timeout = do
    wait sem
    let command = "casperjs --proxy=" ++ ip ++ " --proxy-auth=" ++ auth ++ " \
        \market-page.js '" ++ testUrl ++ "' " ++ timeout ++ " /dev/null"
    print command
    handle <- runCommand command
    exitCode <- waitForProcess handle
    case exitCode of
        ExitSuccess -> do
            atomically $ modifyTVar minTime (modifyMin $ read timeout)
        ExitFailure n -> return ()
    signal sem

writeProxy :: [String] -> IO ()
writeProxy [] = return ()
writeProxy ("":[]) = return ()
writeProxy (timeout:proxy:[])
    | timeout == "-1" = return ()
    | otherwise = do
        proxies <- readFile "proxies.txt"
        let proxies' = lines proxies
        if proxies' == []
            then do
                let newProxies = (proxy:timeout:[])
                writeFile "proxies.txt" $ unlines newProxies
            else do
                let time = last proxies'
                if (read timeout :: Int) < (read time :: Int)
                    then do
                        let newProxies = (init proxies') ++ (proxy:time:[])
                        writeFile "proxies.txt" $ unlines newProxies
                    else do
                        let newProxies = (init proxies') ++ (proxy:timeout:[])
                        writeFile "proxies.txt" $ unlines newProxies

modifyMin :: Int -> Int -> Int
modifyMin new old
    | new == (-1) = old
    | old == (-1) = new
    | new < old = new
    | otherwise = old

splitProxies :: String -> [(String, String)]
splitProxies proxyList = fmap (\x -> (ip $ proxy x, auth $ proxy x)) proxies
    where proxies = lines proxyList
          proxy x = splitOn ":" x
          ip x = (x !! 0) ++ ":" ++ (x !! 1)
          auth x = (x !! 2) ++ ":" ++ (x !! 3)
