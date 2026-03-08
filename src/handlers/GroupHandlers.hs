{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module GroupHandlers(createGroupHandler, getGroupHandler, deleteGroupHandler) where


import Servant
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import Hasql.Connection (Connection)
import qualified Hasql.Pool as P
import Hasql.Pool (Pool)
import qualified Group
import GroupsDTO (GroupsDTO(..))

createGroupHandler :: Pool -> GroupsDTO -> Handler NoContent
createGroupHandler pool body = do
    res <- liftIO $ Group.insertGroup body pool
    case res of
        Left _ -> throwError err500
        Right [] -> throwError err403
        Right _ -> return NoContent

getGroupHandler :: Pool -> Text -> Handler [GroupsDTO]
getGroupHandler pool gid = do
    res <- liftIO $ Group.findGroup gid pool
    case res of
        Left _ -> throwError err500
        Right [] -> throwError err404
        Right as -> return $ map Group.toGroupDTO as

deleteGroupHandler :: Pool -> Text -> Handler NoContent
deleteGroupHandler pool gid = do
    res <- liftIO $ Group.deleteGroup gid pool
    case res of
        Left _ -> throwError err500
        Right [] -> throwError err404
        Right _ -> return NoContent