import Text.HTML.TagSoup
import System.IO
import System.Process
import Paths_steam_market_scraper

data MarketItem = undefined
data ItemListing = undefined

main :: IO()
main = do
    --handle <- runCommand $ ("casperjs market-total.js 'http://steamcommunity.com/market/search?q=appid%3A730'")
    --waitForProcess handle >>= print
    --handle2 <- runCommand $ ("casperjs market-page.js 'http://steamcommunity.com/market/search?q=appid%3A730#p50' csgo-pages/")
    --waitForProcess handle2 >>= print
    
scrapeMarketPage :: Text -> [MarketItem]
undefined

scarapeItemPage :: Text -> [ItemListing]
undefined
