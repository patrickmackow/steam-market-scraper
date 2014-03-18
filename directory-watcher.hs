import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Either.Utils
import Data.List
import Data.List.Split
import System.Directory
import qualified Data.ConfigFile as C
import qualified Data.Set as Set

data Directory = Directory
    { path :: String
    , contents :: [String]
    } deriving (Show, Eq)

type DirectoryS a = StateT [Directory] IO a

main :: IO ()
main = do
    configValues <- liftM forceEither $ C.readfile C.emptyCP
        "conf.d/directory-watcher.conf"
    let directories = splitOn "," $ forceEither $ C.get
            configValues "DEFAULT" "directories"
    print directories
    let directoryList = map (\x -> Directory x []) directories
    print directoryList

    runStateT loop directoryList
    return ()

loop :: DirectoryS ()
loop = do
    directories <- get
    directories' <- liftIO $ execStateT checkDirectories $ directories

    let newFiles = zipWith compareFiles directories directories'
    forM_ newFiles $ \(x,y) -> do
        if length y == 0
            then return ()
            else do
                liftIO $ putStr $ (path x) ++ " "
                liftIO $ print y

    put $ directories'

    liftIO $ threadDelay 1000000

    loop

compareFiles :: Directory -> Directory -> (Directory, [String])
compareFiles x y = (y, Set.toList $ Set.difference newFiles oldFiles)
    where oldFiles = Set.fromList $ contents x
          newFiles = Set.fromList $ contents y

checkDirectories :: DirectoryS ()
checkDirectories = do
    directories <- get
    directories' <- forM directories $ \directory -> do
        let p = path directory
            c = contents directory
        files <- liftIO $ getDirectoryContents p
        let currentFiles = filter (isSuffixOf ".html") files
        return $ Directory p currentFiles
    put directories'
    return ()
