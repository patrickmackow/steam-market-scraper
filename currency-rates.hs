import Data.Aeson
import Network.HTTP
import Network.Stream
import System.IO
import Text.HTML.TagSoup
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T

ratesURLs = [ 
    "http://www.xe.com/currencyconverter/convert/?Amount=1&From=RUB&To=USD",
    "http://www.xe.com/currencyconverter/convert/?Amount=1&From=GBP&To=USD",
    "http://www.xe.com/currencyconverter/convert/?Amount=1&From=BRL&To=USD",
    "http://www.xe.com/currencyconverter/convert/?Amount=1&From=EUR&To=USD"
    ]

data CurrencyRate = CurrencyRate
    { currency :: String
    , rate :: Double
    } deriving (Show)

instance ToJSON CurrencyRate where
    toJSON (CurrencyRate currency rate) = 
        object [T.pack "currency" .= currency, T.pack "rate" .= rate]

main :: IO ()
main = do
    srcs <- mapM openURL ratesURLs
    let rates = fmap scrapeCurrencyPage srcs
    let json = fmap B.unpack $ fmap encode rates
    writeFile "rates.json" $ unlines json

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

scrapeCurrencyPage :: String -> CurrencyRate
scrapeCurrencyPage page = scrapeRate tags
    where tags = head . sections (~== "<tr class=uccRes>") $ parseTags page

scrapeRate :: [Tag String] -> CurrencyRate
scrapeRate tags = CurrencyRate currency $ read rate
    where currency = getTagText . head $ 
            sections (~== "<span class=uccResCde>") tags
          rate = takeWhile (/= '\160') . getTagText . head $ 
            sections (~== "<td class=rightCol>") tags

-- Util functions
getAttrib :: String -> [Tag String] -> String
getAttrib atr tag = fromAttrib atr . head . filter isTagOpen $ tag

getTagText :: [Tag String] -> String
getTagText = fromTagText . head . filter isTagText
