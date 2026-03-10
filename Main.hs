{-# LANGUAGE OverloadedStrings #-}

import Network.Wai.Handler.Warp (run)
import Server (app)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import Hasql.Connection
import Hasql.Pool as P
import Data.Time (NominalDiffTime)

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
            let connSettings = settings (encodeUtf8 $ pack $ dbHost conf)
                                        (fromIntegral $ dbPort conf)
                                        (encodeUtf8 $ pack $ dbUser conf)
                                        (encodeUtf8 $ pack $ dbPassword conf)
                                        (encodeUtf8 $ pack $ dbName conf)
            pool <- P.acquire 10 60 60 60 connSettings
            putStrLn "Starting Servant server on port 3001 "
            run 3001 (app pool)

