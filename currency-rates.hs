import Text.HTML.TagSoup
import Network.HTTP
import Network.Stream

ratesURLs = [ 
    "http://www.xe.com/currencyconverter/convert/?Amount=1&From=RUB&To=USD",
    "http://www.xe.com/currencyconverter/convert/?Amount=1&From=GBP&To=USD",
    "http://www.xe.com/currencyconverter/convert/?Amount=1&From=BRL&To=USD",
    "http://www.xe.com/currencyconverter/convert/?Amount=1&From=EUR&To=USD"
    ]

main :: IO ()
main = do
    srcs <- mapM openURL ratesURLs
    let rates = fmap scrapeCurrencyPage srcs
    print rates
    -- let tags = map parseTags srcmap (sections (~== "<tr class=uccRes>")) tags

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

scrapeCurrencyPage :: String -> (String, String)
scrapeCurrencyPage page = scrapeRate tags
    where tags = head . sections (~== "<tr class=uccRes>") $ parseTags page

scrapeRate :: [Tag String] -> (String, String)
scrapeRate tags = (currency, rate)
    where currency = getTagText . head $ sections (~== "<span class=uccResCde>") tags
          rate = getTagText . head $ sections (~== "<td class=rightCol>") tags

-- Util functions
getAttrib :: String -> [Tag String] -> String
getAttrib atr tag = fromAttrib atr . head . filter isTagOpen $ tag

getTagText :: [Tag String] -> String
getTagText = fromTagText . head . filter isTagText
