import Control.Concurrent
import Control.Concurrent.SSem
import Control.Concurrent.STM
import Control.Monad
import Data.Either.Utils
import Data.List.Split
import Network.HTTP
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

    -- Get list of proxies from api
    proxyList <- openURL api
    let proxies = splitProxies proxyList

    sem <- new $ length proxies

    mapM_ forkIO $ map (testProxy sem timeout) proxies

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

testProxy :: SSem -> [String] -> (String, String) -> IO ()
testProxy sem timeout proxy = do
    wait sem

    sem2 <- new $ length timeout
    time <- newTVarIO (-1)

    mapM_ forkOS $ map (benchmark sem2 time proxy) timeout

    signal sem

benchmark :: SSem -> TVar Int -> (String, String) -> String -> IO ()
benchmark = undefined

splitProxies :: String -> [(String, String)]
splitProxies proxyList = fmap (\x -> (ip $ proxy x, auth $ proxy x)) proxies
    where proxies = lines proxyList
          proxy x = splitOn ":" x
          ip x = (x !! 0) ++ ":" ++ (x !! 1)
          auth x = (x !! 2) ++ ":" ++ (x !! 3)
