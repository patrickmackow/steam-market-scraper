import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Either.Utils
import Data.List
import Data.List.Split
import System.Directory
import System.Exit
import System.Process
import qualified Data.ConfigFile as C
import qualified Data.Set as Set

data Directory = Directory
    { path :: String
    , fileType :: String
    , contents :: [String]
    } deriving (Show, Eq)

type DirectoryS a = StateT [Directory] IO a

main :: IO ()
main = do
    configValues <- liftM forceEither $ C.readfile C.emptyCP
        "conf.d/directory-watcher.conf"
    let values = parseConf $ splitOn "," $ forceEither $ C.get
            configValues "DEFAULT" "directories"
    print values
    let directoryList = map (\(p, f) -> Directory p f []) values
    mapM_ (\(p, f) -> createDirectoryIfMissing False (p ++ "failed")) values
    print directoryList

    runStateT loop directoryList
    return ()

parseConf :: [String] -> [(String, String)]
parseConf c = fmap f $ split c
    where split = fmap (splitOn ":")
          f :: [String] -> (String, String)
          f (p:f:[]) = (p, f)

loop :: DirectoryS ()
loop = do
    directories <- get
    directories' <- liftIO $ execStateT checkDirectories $ directories

    let newFiles = zipWith compareFiles directories directories'
    forM_ newFiles $ \(x,y) -> do
        if length y == 0
            then return ()
            else do
                liftIO $ mapM_
                    (\file -> processFile (fileType x) (path x) file) y

    put $ directories'

    liftIO $ threadDelay 1000000

    loop

processFile :: String -> FilePath -> FilePath -> IO ()
processFile fileType dir path = do
    let filePath = dir ++ path
        command = "./dist/build/file-scraper/file-scraper " ++ fileType
            ++ " " ++ filePath
    handle <- runCommand command
    exitCode <- waitForProcess handle
    case exitCode of
        ExitSuccess -> removeFile filePath
        ExitFailure n -> do
            renameFile filePath (dir ++ "failed/" ++ path)
            print $ "ERROR: " ++ (show n) ++ " " ++ path

compareFiles :: Directory -> Directory -> (Directory, [String])
compareFiles x y = (y, Set.toList $ Set.difference newFiles oldFiles)
    where oldFiles = Set.fromList $ contents x
          newFiles = Set.fromList $ contents y

checkDirectories :: DirectoryS ()
checkDirectories = do
    directories <- get
    directories' <- forM directories $ \directory -> do
        let p = path directory
            f = fileType directory
            c = contents directory
        files <- liftIO $ getDirectoryContents p
        let currentFiles = filter (isSuffixOf ".html") files
        return $ Directory p f currentFiles
    put directories'
    return ()
