import Text.HTML.TagSoup
import System.IO
import System.Process
import System.Directory
import Data.Char
import Data.List
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types
import Data.Int
import qualified Data.ByteString.Char8 as B
import Control.Applicative
import Control.Monad

import Paths_steam_listing_scraper

data MarketItem = MarketItem { url :: String
                             , image :: String
                             , quantity :: String
                             , price :: String
                             , name :: String
                             , nameColour :: String
                             , game :: String
                             } deriving (Show, Eq)

data ItemListing = ItemListing { listingNo :: String
                               , url :: String
                               , price :: String
                               , priceBeforeFee :: String
                               } deriving (Show, Eq)

instance ToRow MarketItem where
    toRow m = [toField (url m), toField (image m), toField (quantity m), 
        toField (price m), toField (name m), toField (nameColour m), 
        toField (game m)]

instance FromRow MarketItem where
    fromRow = MarketItem <$> field <*> field <*> field 
        <*> field <*> field <*> field <*> field

instance ToRow ItemListing where
    toRow i = [toField (listingNo i), toField (url i), toField (price i),
        toField (priceBeforeFee i)]

instance FromRow ItemListing where
    fromRow = ItemListing <$> field <*> field <*> field <*> field

main :: IO ()
main = do
    -- Download all market pages
    total <- readProcess "casperjs" ["market-total.js"
        ,"http://steamcommunity.com/market/search?q=appid%3A730"] []
    scrapeMarket $ read . head . lines $ total

    files <- getDirectoryContents "csgo-pages/"
    let pages = filter (all isDigit) files
    items <- mapM (\x -> readMarketPage x) 
        $ fmap (\x -> "csgo-pages/" ++ x) pages

    conn <- connect defaultConnectInfo { connectUser = "patrick"
                                       , connectPassword = ""
                                       , connectDatabase = "steam_market" }
    storeItems conn $ nub $ concat items
    -- count <- insertItems conn (nub $ concat items)
    -- print count
    close conn
    -- test <- readFile "csgo-pages/50"
    -- let items = scrapeMarketPage test
    -- print items
    -- print $ length items

-- Download all Steam Market pages
scrapeListings :: Int -> IO ()
scrapeListings 1 = do
    handle <- runCommand $ ("casperjs market-page.js\
    \ 'http://steamcommunity.com/market/search?q=appid%3A730#p"
        ++ (show 1) ++ "' csgo-pages/")
    waitForProcess handle
    return ()
scrapeListings x = do
    handle <- runCommand $ ("casperjs market-page.js\
    \ 'http://steamcommunity.com/market/search?q=appid%3A730#p"
        ++ (show x) ++ "' csgo-pages/")
    waitForProcess handle
    scrapeMarket (x - 1)

readListingsPage :: FilePath -> IO [ItemListing]
readListingsPage path = do
    file <- readFile path
    -- removeFile path
    return $ scrapeListingsPage file

storeListings :: Connection -> [MarketItem] -> IO ()
storeListings conn items = do
    let select = Query $ B.pack "SELECT url FROM market"
    existing <- query_ conn select
    let existingStrings = fmap (\(Only url) -> B.unpack url) existing
    -- forM_ existing $ \(Only url) -> print $ B.unpack url
    print $ length existing
    forM_ items $ \item -> checkItem conn existingStrings item
    return ()

insertListings :: Connection -> MarketItem -> IO Int64
insertListings conn item = execute conn insert item
    where insert = Query $ B.pack "INSERT INTO market (url, image, \
             \quantity, price, item_name, item_name_colour, game) \
             \VALUES (?,?,?,?,?,?,?)"

-- Scrape info from market page
scrapeListingsPage :: String -> [ItemListing]
scrapeListingsPage page = fmap scrapeItemListing listings
    where listings = sections (~== "<div class=market_listing_row>")
                  $ parseTags page

scrapeItemListing :: [Tag String] -> ItemListing
scrapeItemListing listing = 
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

-- Util functions
getAttrib :: String -> [Tag String] -> String
getAttrib atr tag = fromAttrib atr . head . filter isTagOpen $ tag

getTagText :: [Tag String] -> String
getTagText = fromTagText . head . filter isTagText
