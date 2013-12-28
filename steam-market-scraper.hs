import Text.HTML.TagSoup
import System.IO
import System.Process
import Paths_steam_market_scraper

main :: IO()
main = do
    marketTotal <- getDataFileName "market-total.js" 
    print $ (marketTotal ++ " 'http://steamcommunity.com/market/search?q=appid%3A730'")
    handle <- runCommand $ ("casperjs " ++ marketTotal ++ " 'http://steamcommunity.com/market/search?q=appid%3A730'")
    exitCode <- waitForProcess handle
    print exitCode
    handle2 <- runCommand $ ("casperjs market-total.js 'http://steamcommunity.com/market/search?q=appid%3A730'")
    waitForProcess handle2 >>= print
