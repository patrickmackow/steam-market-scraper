module Utils where

import Data.Either.Utils
import Database.PostgreSQL.Simple
import qualified Data.ConfigFile as C

getConnectInfo :: IO ConnectInfo
getConnectInfo = do
    val <- C.readfile C.emptyCP "conf.d/sms.conf"
    let cp = forceEither val
    let database = forceEither $ C.get cp "DEFAULT" "database"
    let db_host = forceEither $ C.get cp "DEFAULT" "db_host"
    let db_port = forceEither $ C.get cp "DEFAULT" "db_port"
    let db_user = forceEither $ C.get cp "DEFAULT" "db_user"
    let db_pass = forceEither $ C.get cp "DEFAULT" "db_pass"
    return $ ConnectInfo db_host db_port db_user db_pass database
