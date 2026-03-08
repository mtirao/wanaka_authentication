{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module MapHandlers(createMapHandler, getMapHandler, deleteMapHandler) where

import Servant
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import Hasql.Connection (Connection)
import qualified Hasql.Pool as P
import Hasql.Pool (Pool)
import qualified ResourceMap
import ResourceMapDTO (ResourceMapDTO(..))
import Hasql.Session (QueryError)
import Rel8 (Result)

createMapHandler :: Pool -> ResourceMapDTO -> Handler NoContent
createMapHandler pool body = do
    res <- liftIO $ ResourceMap.insertResourceMap body pool
    case res of     
        Left _ -> throwError err500
        Right [] -> throwError err403
        Right _ -> return NoContent 

getMapHandler :: Pool -> Text -> Handler ResourceMapDTO
getMapHandler pool rid = do 
    res <- liftIO $ getMap rid pool
    case res of
        Left _ -> throwError err500    
        Right Nothing -> throwError err404
        Right (Just m) -> return $ ResourceMap.toResourceMapDTO m 

deleteMapHandler :: Pool -> Text -> Handler NoContent
deleteMapHandler pool rid = do
    res <- liftIO $ ResourceMap.deleteResourceMap rid pool
    case res of
        Left _ -> throwError err500
        Right [] -> throwError err404
        Right _ -> return NoContent 

-- Helper function to get a map by resource
getMap :: Text -> Pool -> IO (Either P.UsageError (Maybe (ResourceMap.ResourceMap Result)))
getMap resource pool = do
    res <- ResourceMap.findResourceMap resource pool
    case res of
        Left err -> return $ Left err
        Right [] -> return $ Right Nothing
        Right (m:_) -> return $ Right (Just m)