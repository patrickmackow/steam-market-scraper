import Text.HTML.TagSoup
import System.IO
import System.Process

import Paths_steam_market_scraper

data MarketItem = MarketItem { link :: String
                             , image :: String
                             , quantity :: String
                             , price :: String
                             , name :: String
                             , nameColour :: String
                             , game :: String
                             } deriving (Show, Eq)

main :: IO()
main = do
    -- handle <- runCommand $ ("casperjs market-total.js\
    -- \'http://steamcommunity.com/market/search?q=appid%3A730'")
    -- waitForProcess handle >>= print
    -- handle2 <- runCommand $ ("casperjs market-page.js\
    -- \'http://steamcommunity.com/market/search?q=appid%3A730#p50'\
    -- \csgo-pages/")
    -- waitForProcess handle2 >>= print
    test <- readFile "csgo-pages/50"
    let items = scrapeMarketPage test
    print items
    print $ length items

scrapeMarketPage :: String -> [MarketItem]
scrapeMarketPage page = fmap scrapeMarketItem items
    where items = sections (~== "<a class=market_listing_row_link>")
                  $ parseTags page

scrapeMarketItem :: [Tag String] -> MarketItem
scrapeMarketItem item = 
    MarketItem link image quantity price name nameColour game
    where link = scrapeLink item
          image = scrapeImage item
          quantity = scrapeQuantity item
          price = scrapePrice item
          name = scrapeName item
          nameColour = scrapeNameColour item
          game = scrapeGame item

scrapeLink :: [Tag String] -> String
scrapeLink item = getAttrib "href" item

scrapeImage :: [Tag String] -> String
scrapeImage item = getAttrib "src" image
    where image = head . sections (~== "<img>") $ item

scrapeQuantity :: [Tag String] -> String
scrapeQuantity item = getTagText quantity
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
          g x = takeWhile (/= ' ') $ dropWhile (/= '$') x

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
