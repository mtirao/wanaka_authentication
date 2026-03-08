{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module PermissionHandlers(createPermissionHandler, getPermissionHandler, deletePermissionHandler) where

import Servant
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import Hasql.Connection (Connection)
import qualified Hasql.Pool as P
import Hasql.Pool (Pool)
import qualified UserPermissions
import UserPermissionsDTO (UserPermissionsDTO(..))



createPermissionHandler :: Pool -> UserPermissionsDTO -> Handler NoContent
createPermissionHandler pool body = do
    res <- liftIO $ UserPermissions.insertUserPermission body pool
    case res of
        Left _ -> throwError err500
        Right [] -> throwError err403
        Right _ -> return NoContent     

getPermissionHandler :: Pool -> Text -> Handler [UserPermissionsDTO]
getPermissionHandler pool rid = do
    res <- liftIO $ UserPermissions.findUserPermission rid pool
    case res of 
        Left _ -> throwError err500
        Right [] -> throwError err404
        Right as -> return $ map UserPermissions.toUserPermissionsDTO as     

deletePermissionHandler :: Pool -> Text -> Handler NoContent
deletePermissionHandler pool rid = do
    res <- liftIO $ UserPermissions.deleteUserPermission rid pool
    case res of
        Left _ -> throwError err500
        Right [] -> throwError err404
        Right _ -> return NoContent 