{-# LANGUAGE OverloadedStrings #-}

import Network.Wai.Handler.Warp (run)
import Server (app)
import Data.Text (pack)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import qualified Hasql.Connection.Setting as Setting
import qualified Hasql.Connection.Setting.Connection as ConnSetting
import Hasql.Pool as P
import qualified Hasql.Pool.Config as PoolConfig

data DbConfig = DbConfig
    { dbName     :: String
    , dbUser     :: String
    , dbPassword :: String
    , dbHost     :: String
    , dbPort     :: Int
    }

makeDbConfig :: CT.Config -> IO (Maybe DbConfig)
makeDbConfig conf = do
    dbConfname <- C.lookup conf "database.name" :: IO (Maybe String)
    dbConfUser <- C.lookup conf "database.user" :: IO (Maybe String)
    dbConfPassword <- C.lookup conf "database.password" :: IO (Maybe String)
    dbConfHost <- C.lookup conf "database.host" :: IO (Maybe String)
    dbConfPort <- C.lookup conf "database.port" :: IO (Maybe Int)
    return $ DbConfig <$> dbConfname
                      <*> dbConfUser
                      <*> dbConfPassword
                      <*> dbConfHost
                      <*> dbConfPort

main :: IO ()
main = do
    loadedConf <- C.load [C.Required "application.conf"]
    dbConf <- makeDbConfig loadedConf
    case dbConf of
        Nothing -> putStrLn "Error loading configuration"
        Just conf -> do
            let connString = pack $ "host=" ++ dbHost conf ++ " port=" ++ show (dbPort conf)
                                ++ " user=" ++ dbUser conf ++ " password=" ++ dbPassword conf
                                ++ " dbname=" ++ dbName conf
                connSettings = Setting.connection (ConnSetting.string connString)
            pool <- P.acquire $ PoolConfig.settings
                [ PoolConfig.size 10
                , PoolConfig.acquisitionTimeout 60
                , PoolConfig.agingTimeout 60
                , PoolConfig.idlenessTimeout 60
                , PoolConfig.staticConnectionSettings [connSettings]
                ]
            putStrLn "Starting Servant server on port 3001 "
            run 3001 (app pool)

