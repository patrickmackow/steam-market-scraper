import Control.Concurrent
import Control.Monad
import Data.Either.Utils
import Data.List.Split
import qualified Data.ConfigFile as C

data Directory = Directory
    { path :: String
    , contents :: [String]
    } deriving (Show, Eq)

main :: IO ()
main = do
    configValues <- liftM forceEither $ C.readfile C.emptyCP
        "conf.d/directory-watcher.conf"
    let directories = splitOn "," $ forceEither $ C.get
            configValues "DEFAULT" "directories"
    print directories

    forever $ do
        threadDelay 1000000
