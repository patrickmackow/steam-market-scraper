{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad.IO.Class
import Data.Aeson
import Data.Either.Utils
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Network.Wai.Middleware.Static
import System.Directory
import System.Process
import Web.Scotty
import qualified Data.ConfigFile as C
import qualified Data.List as L

data Underpriced = Underpriced
    { itemId :: Int
    , listingNo :: String
    , listingId :: Int
    , url :: String
    , price :: Int
    , priceBeforeFee :: Int
    , profit :: Int
    , image :: String
    , itemName :: String
    , itemNameColour :: String
    , game :: String
    } deriving (Show, Eq)

instance FromRow Underpriced where
    fromRow = Underpriced <$> field <*> field <*> field <*> field <*> field
        <*> field <*> field <*> field <*> field <*> field <*> field

instance ToJSON Underpriced where
    toJSON (Underpriced itemId listingNo listingId url price priceBeforeFee 
        profit image itemName itemNameColour game) =
        object ["itemId" .= itemId, "listingNo" .= listingNo,
        "listingId" .= listingId, "url" .= url, "price" .= price,
        "priceBeforeFee" .= priceBeforeFee, "profit" .= profit,
        "image" .= image, "itemName" .= itemName,
        "itemNameColour" .= itemNameColour, "game" .= game]

data Timestamp = Timestamp { timestamp :: String } deriving (Show, Eq)

instance FromRow Timestamp where
    fromRow = Timestamp <$> field

instance ToJSON Timestamp where
    toJSON (Timestamp timestamp) = object ["timestamp" .= timestamp]

main = scotty 3000 $ do
    -- Unsafe add this to static folder
    middleware $ staticPolicy (noDots >-> addBase "static")
    connInfo <- liftIO $ getConnectInfo

    get "/sms/" $ do
        setHeader "Content-Type" "text/html"
        file "index.html"

    -- Return JSON with all underpriced items
    get "/sms/underpriced/all/" $ do
        let select = "SELECT u.id, u.listing_no, u.listing_id, u.url, \
            \u.item_price, l.item_price_before_fee, \
            \(l.item_price_before_fee - u.item_price) AS profit, \
            \m.image, m.item_name, m.item_name_colour, m.game \
            \FROM underpriced AS u, listing_history AS l, market AS m \
            \WHERE u.listing_id = l.id AND u.url = m.url \
            \ORDER BY u.id ASC;"
        conn <- liftIO $ connect connInfo
        under <- liftIO $ (query_ conn select :: IO [Underpriced])
        liftIO $ close conn
        Web.Scotty.json under

    -- Return JSON with single underpriced items
    get "/sms/underpriced/single/" $ do
        let select = "SELECT u.id, u.listing_no, u.listing_id, u.url, \
            \u.item_price, l.item_price_before_fee, \
            \(l.item_price_before_fee - u.item_price) AS profit, \
            \m.image, m.item_name, m.item_name_colour, m.game \
            \FROM underpriced AS u, listing_history AS l, market AS m \
            \WHERE u.listing_id = l.id AND u.url = m.url \
            \AND u.listing_id IN (SELECT listing_id FROM underpriced \
            \GROUP BY listing_id HAVING COUNT(*) = 1) \
            \AND NOT EXISTS (SELECT underpriced_no FROM excluded AS e \
            \WHERE u.listing_no = e.underpriced_no) \
            \AND (l.item_price_before_fee - u.item_price) >= 5 \
            \ORDER BY u.id ASC;"
        conn <- liftIO $ connect connInfo
        under <- liftIO $ (query_ conn select :: IO [Underpriced])
        liftIO $ close conn
        Web.Scotty.json under

    -- Return JSON with all new underpriced items
    get "/sms/underpriced/all/:listing" $ do
        listing <- param "listing"
        let select = "SELECT u.id, u.listing_no, u.listing_id, u.url, \
            \u.item_price, l.item_price_before_fee, \
            \(l.item_price_before_fee - u.item_price) AS profit, \
            \m.image, m.item_name, m.item_name_colour, m.game \
            \FROM underpriced AS u, listing_history AS l, market AS m \
            \WHERE u.listing_id = l.id AND u.url = m.url \
            \AND u.id > ? \
            \ORDER BY u.id ASC;"
        conn <- liftIO $ connect connInfo
        under <- liftIO $ (query conn select
            (listing:[] :: [Int]) :: IO [Underpriced])
        liftIO $ close conn
        Web.Scotty.json under

    -- Return JSON with new single underpriced items
    get "/sms/underpriced/single/:listing" $ do
        listing <- param "listing"
        let select = "SELECT u.id, u.listing_no, u.listing_id, u.url, \
            \u.item_price, l.item_price_before_fee, \
            \(l.item_price_before_fee - u.item_price) AS profit, \
            \m.image, m.item_name, m.item_name_colour, m.game \
            \FROM underpriced AS u, listing_history AS l, market AS m \
            \WHERE u.listing_id = l.id AND u.url = m.url \
            \AND u.listing_id IN (SELECT listing_id FROM underpriced \
            \GROUP BY listing_id HAVING COUNT(*) = 1) \
            \AND NOT EXISTS (SELECT underpriced_no FROM excluded AS e \
            \WHERE u.listing_no = e.underpriced_no) \
            \AND (l.item_price_before_fee - u.item_price) >= 5 \
            \AND u.id > ? \
            \ORDER BY u.id ASC;"
        conn <- liftIO $ connect connInfo
        under <- liftIO $ (query conn select
            (listing:[] :: [Int]) :: IO [Underpriced])
        liftIO $ close conn
        Web.Scotty.json under

    get "/sms/excluded/" $ do
        let select = "SELECT u.id, u.listing_no, u.listing_id, u.url, \
            \u.item_price, l.item_price_before_fee, \
            \(l.item_price_before_fee - u.item_price) AS profit, \
            \m.image, m.item_name, m.item_name_colour, m.game \
            \FROM underpriced AS u, listing_history AS l, market AS m, \
            \excluded AS e \
            \WHERE u.listing_id = l.id AND u.url = m.url \
            \AND u.listing_no = e.underpriced_no \
            \ORDER BY u.id ASC;"
        conn <- liftIO $ connect connInfo
        excluded <- liftIO $ (query_ conn select :: IO [Underpriced])
        liftIO $ close conn
        Web.Scotty.json excluded

    put "/sms/excluded/:listing" $ do
        listing <- param "listing"
        let select = "SELECT u.id, u.listing_no, u.listing_id, u.url, \
            \u.item_price, l.item_price_before_fee, \
            \(l.item_price_before_fee - u.item_price) AS profit, \
            \m.image, m.item_name, m.item_name_colour, m.game \
            \FROM underpriced AS u, listing_history AS l, market AS m, \
            \excluded AS e \
            \WHERE u.listing_id = l.id AND u.url = m.url \
            \AND e.underpriced_no = ?"
        conn <- liftIO $ connect connInfo
        existing <- liftIO $ (query conn select
            (listing:[] :: [String]) :: IO [Underpriced])
        if length existing == 0
            then do
                let insert = "INSERT INTO excluded (underpriced_no) VALUES (?)"
                liftIO $ execute conn insert $ listing:[]
                liftIO $ close conn
                return ()
            else do
                return ()

    delete "/sms/excluded/:listing" $ do
        listing <- param "listing"
        let delete = "DELETE FROM excluded WHERE underpriced_no = ?"
        conn <- liftIO $ connect connInfo
        liftIO $ execute conn delete $ (listing:[] :: [String])
        liftIO $ close conn
        return ()

    get "/sms/last/" $ do
        let select = "SELECT latest_timestamp FROM last_run ORDER BY id DESC \
            \LIMIT 1"
        conn <- liftIO $ connect connInfo
        timestamp <- liftIO $ (query_ conn select :: IO [Timestamp])
        liftIO $ close conn
        Web.Scotty.json timestamp

    post "/sms/scrape/" $ do
        liftIO scrape

getConnectInfo :: IO ConnectInfo
getConnectInfo = do
    val <- C.readfile C.emptyCP "sms.conf"
    let cp = forceEither val
    let database = forceEither $ C.get cp "DEFAULT" "database"
    let db_host = forceEither $ C.get cp "DEFAULT" "db_host"
    let db_port = forceEither $ C.get cp "DEFAULT" "db_port"
    let db_user = forceEither $ C.get cp "DEFAULT" "db_user"
    let db_pass = forceEither $ C.get cp "DEFAULT" "db_pass"
    return $ ConnectInfo db_host db_port db_user db_pass database

scrape :: IO ()
scrape = do
    let scraperStatus = "scraper_status.log"
    fileExists <- doesFileExist scraperStatus
    if fileExists
        then do
            status <- readFile scraperStatus
            if scraperReady $ lines status
                then do
                    runCommand "../scrape-listings.sh > /dev/null"
                    return ()
                else return ()
        else return ()

scraperReady :: [String] -> Bool
scraperReady [] = False
scraperReady (x:[]) = L.isPrefixOf "OK" x
scraperReady xs = L.isPrefixOf "OK" $ last xs
