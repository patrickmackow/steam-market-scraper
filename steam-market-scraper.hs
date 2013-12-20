import Paths_steam_market_scraper

main :: IO()
main = getDataFileName "market-total.js" >>= print
