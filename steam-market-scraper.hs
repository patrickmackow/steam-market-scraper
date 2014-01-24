import Text.HTML.TagSoup
import System.IO
import System.Process
import System.Directory
import Data.Char
import Data.List
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types
import Data.Int
import qualified Data.ByteString.Char8 as B

import Paths_steam_market_scraper

data MarketItem = MarketItem { url :: String
                             , image :: String
                             , quantity :: String
                             , price :: String
                             , name :: String
                             , nameColour :: String
                             , game :: String
                             } deriving (Show, Eq)

instance ToRow MarketItem where
    toRow m = [toField (url m), toField (image m), toField (quantity m), 
        toField (price m), toField (name m), toField (nameColour m), 
        toField (game m)]

main :: IO ()
main = do
    -- Download all market pages
    -- total <- readProcess "casperjs" ["market-total.js"
    --     ,"http://steamcommunity.com/market/search?q=appid%3A730"] []
    -- scrapeMarket $ read . head . lines $ total
    files <- getDirectoryContents "csgo-pages/"
    let pages = filter (all isDigit) files
    items <- mapM (\x -> readMarketPage x) 
        $ fmap (\x -> "csgo-pages/" ++ x) pages

    conn <- connect defaultConnectInfo { connectUser = "patrick"
                                       , connectPassword = ""
                                       , connectDatabase = "steam_market" }
    count <- insertItems conn (nub $ concat items)
    print count
    close conn
    -- test <- readFile "csgo-pages/50"
    -- let items = scrapeMarketPage test
    -- print items
    -- print $ length items

-- Download all Steam Market pages
scrapeMarket :: Int -> IO ()
scrapeMarket 1 = do
    handle <- runCommand $ ("casperjs market-page.js\
    \ 'http://steamcommunity.com/market/search?q=appid%3A730#p"
        ++ (show 1) ++ "' csgo-pages/")
    waitForProcess handle
    return ()
scrapeMarket x = do
    handle <- runCommand $ ("casperjs market-page.js\
    \ 'http://steamcommunity.com/market/search?q=appid%3A730#p"
        ++ (show x) ++ "' csgo-pages/")
    waitForProcess handle
    scrapeMarket (x - 1)

readMarketPage :: FilePath -> IO [MarketItem]
readMarketPage path = do
    file <- readFile path
    return $ scrapeMarketPage file

insertItems :: ToRow q => Connection -> [q] -> IO Int64
insertItems conn items = executeMany conn insert items
    where insert = Query $ B.pack "INSERT INTO market (url, image, \
                   \quantity, price, item_name, item_name_colour, game) \
                   \VALUES (?,?,?,?,?,?,?)"

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
