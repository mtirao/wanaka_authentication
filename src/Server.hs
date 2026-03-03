{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Server (app) where

import Servant
import Network.Wai (Application, Middleware, Request(..), responseLBS, strictRequestBody, getRequestBodyChunk)
import Network.HTTP.Types (status401)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString as B
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Control.Monad (when)
import Data.IORef (newIORef, readIORef, modifyIORef, writeIORef)
import qualified Data.Text.Encoding as TE
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.Wai.Middleware.HttpAuth (extractBasicAuth, extractBearerAuth)

import Hasql.Connection (Connection)
import Hasql.Session (QueryError)

import Rel8 (Result)

import qualified Token as Tkn
import qualified Realm
import qualified Tenant
import qualified Group
import qualified ResourceMap
import qualified UserPermissions
import TokenService (buildToken, toInt64, tokenExperitionTime, decodeToken)
import TokenModel (TokenResponse(..))
import GroupsDTO (GroupsDTO(..))
import UserPermissionsDTO (UserPermissionsDTO(..), UserAuthorizationRequestDTO(..), UserAuthorizationDTO(..)    )
import ResourceMapDTO (ResourceMapDTO(..))


-- API definition
type API =
        "api" :> "wanaka" :> "accounts" :> "login" 
            :> Header "Authorization" Text 
            :> Get '[JSON] TokenResponse
    :<|> "api" :> "wanaka" :> "token" 
            :> Header "x-client-id" Text 
            :> Header "x-client-secret" Text 
            :> Header "x-grant-type" Text 
            :> Get '[JSON] TokenResponse
    :<|> "api" :> "wanaka" :> "token" :> "validate" 
            :> Header "Authorization" Text 
            :> Header "x-client-id" Text 
            :> Get '[JSON] TokenResponse
    :<|> "api" :> "wanaka" :> "group" 
            :> ReqBody '[JSON] GroupsDTO 
            :> Post '[JSON] NoContent
    :<|> "api" :> "wanaka" :> "group" 
            :> Capture "id" Text 
            :> Get '[JSON] [GroupsDTO]
    :<|> "api" :> "wanaka" :> "group" 
            :> Capture "id" Text 
            :> Delete '[JSON] NoContent
    :<|> "api" :> "wanaka" :> "permission" 
            :> ReqBody '[JSON] UserPermissionsDTO 
            :> Post '[JSON] NoContent
    :<|> "api" :> "wanaka" :> "permission" 
            :> Capture "resource" Text 
            :> Get '[JSON] [UserPermissionsDTO]
    :<|> "api" :> "wanaka" :> "permission" 
            :> Capture "resource" Text 
            :> Delete '[JSON] NoContent
    :<|> "api" :> "wanaka" :> "authorize"
            :> ReqBody '[JSON] UserAuthorizationRequestDTO
            :> Post '[JSON] UserAuthorizationDTO  
    :<|> "api" :> "wanaka" :> "map" 
            :> ReqBody '[JSON] ResourceMapDTO 
            :> Post '[JSON] NoContent
    :<|> "api" :> "wanaka" :> "map"
            :> Capture "resource" Text
            :> Get '[JSON] ResourceMapDTO
    :<|> "api" :> "wanaka" :> "map"
            :> Capture "resource" Text
            :> Delete '[JSON] NoContent 



api :: Proxy API
api = Proxy

-- Application entrypoint
app :: Connection -> Application
app conn = loggingMiddleware $ tokenMiddleware conn $ serve api (server conn)

-- Server implementation
server :: Connection -> Server API
server conn = loginHandler conn
         :<|> tokenCreateHandler conn
         :<|> tokenValidateHandler conn
         :<|> createGroupHandler conn
         :<|> getGroupHandler conn
         :<|> deleteGroupHandler conn
         :<|> createPermissionHandler conn
         :<|> getPermissionHandler conn
         :<|> deletePermissionHandler conn
         :<|> authorizeHandler conn
         :<|> createMapHandler conn
         :<|> getMapHandler conn
         :<|> deleteMapHandler conn 


-- Middleware and helpers

-- | Simple token check using same logic as tokenValidateHandler (minus clientId)
checkToken :: Connection -> Text -> IO Bool
checkToken conn t = case decodeToken t of
    Nothing -> return False
    Just payload -> do
        cur <- liftIO getPOSIXTime
        if tokenExperitionTime payload >= toInt64 cur then do
            res <- Tkn.findToken t conn
            case res of
                Left _ -> return False
                Right [] -> return False
                Right _  -> return True
        else return False

-- | Determine if the request path should skip authentication
skipAuth :: [Text] -> Bool
skipAuth path = path == ["api","wanaka","accounts","login"]
               || path == ["api","wanaka","token","validate"]

-- | Middleware that validates bearer token on all endpoints except login/validate
loggingMiddleware :: Middleware
loggingMiddleware app req respond = do
    -- capture time
    now <- getCurrentTime
    let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
    -- read and store body for replay
    chunksRef <- newIORef []
    let pullBody acc = do
            chunk <- getRequestBodyChunk req
            if B.null chunk then return (reverse acc)
            else pullBody (chunk:acc)
    chunks <- pullBody []
    writeIORef chunksRef chunks
    let bodyBS = B.concat chunks
    -- log metadata
    let method = BS.unpack (requestMethod req)
        path   = show (pathInfo req)
        hdrs   = show (requestHeaders req)
        qs     = show (queryString req)
        bodyStr = show bodyBS
    putStrLn $ "[Request] " ++ timeStr ++ " " ++ method ++ " " ++ path
    putStrLn $ "  headers: " ++ hdrs
    putStrLn $ "  query: " ++ qs
    putStrLn $ "  body: " ++ bodyStr
    -- rebuild request body for downstream handlers
    let req' = req { requestBody = do
                        hs <- readIORef chunksRef
                        case hs of
                            [] -> return B.empty
                            (h:rest) -> do
                                writeIORef chunksRef rest
                                return h
                    }
    app req' respond


tokenMiddleware :: Connection -> Middleware
tokenMiddleware conn app req respond =
    if skipAuth (pathInfo req) then
        app req respond
    else case lookup "authorization" (requestHeaders req) of
        Nothing -> respond $ responseLBS status401 [("Content-Type","text/plain")] "Unauthorized"
        Just auth -> case extractBearerAuth auth of
            Nothing -> respond $ responseLBS status401 [("Content-Type","text/plain")] "Unauthorized"
            Just tok -> do
                let tokText = TE.decodeUtf8 tok
                valid <- checkToken conn tokText
                if valid then app req respond
                         else respond $ responseLBS status401 [("Content-Type","text/plain")] "Unauthorized"

-- Handlers
loginHandler :: Connection -> Maybe Text -> Handler TokenResponse
loginHandler conn mAuth = case mAuth of
    Nothing -> throwError err401
    Just auth -> case extractBasicAuth (TE.encodeUtf8 auth) of
        Nothing -> throwError err401
        Just (u, p) -> do
            let userText = TE.decodeUtf8 u
            let passText = TE.decodeUtf8 p
            res <- liftIO $ Tenant.findTenant userText passText conn
            case res of
                Left _ -> throwError err403
                Right [] -> throwError err403
                Right [t] -> do
                    cur <- liftIO getPOSIXTime
                    let expTime = toInt64 cur + 864000
                    let token = buildToken "client_credentials" (Tenant.getUserId t) expTime
                    _ <- liftIO $ Tkn.insertToken token "client_credentials" conn
                    return $ TokenResponse token "JWT" ""

tokenCreateHandler :: Connection -> Maybe Text -> Maybe Text -> Maybe Text -> Handler TokenResponse
tokenCreateHandler conn mClientId mClientSecret mGrant = case (mClientId, mClientSecret, mGrant) of
    (Just cid, Just csecret, Just _gtype) -> do
        res <- liftIO $ Realm.findRealm csecret conn
        case res of
            Left _ -> throwError err500
            Right [] -> throwError err401
            Right [r] -> do
                let grantType = Realm.getGrantType r
                let clientid = Realm.getClientId r
                if cid == clientid then do
                    cur <- liftIO getPOSIXTime
                    let expTime = toInt64 cur + 864000
                    let token = buildToken grantType clientid expTime
                    _ <- liftIO $ Tkn.insertToken token grantType conn
                    return $ TokenResponse token "JWT" ""
                else throwError err401
    _ -> throwError err400

tokenValidateHandler :: Connection -> Maybe Text -> Maybe Text -> Handler TokenResponse
tokenValidateHandler conn mAuth mClientId = case (mAuth, mClientId) of
    (Just auth, Just clientId) -> case extractBearerAuth (TE.encodeUtf8 auth) of
        Nothing -> throwError err401
        Just tok -> case decodeToken (TE.decodeUtf8 tok) of
            Nothing -> throwError err401
            Just payload -> do
                cur <- liftIO getPOSIXTime
                if tokenExperitionTime payload >= toInt64 cur then do
                    res <- liftIO $ Tkn.findToken (TE.decodeUtf8 tok) conn
                    case res of
                        Left _ -> throwError err500
                        Right [] -> throwError err401
                        Right [t] -> if Tkn.getClientId t == clientId then
                                        return $ TokenResponse (TE.decodeUtf8 tok) "JWT" ""
                                     else throwError err401
                else throwError err401
    _ -> throwError err400

createGroupHandler :: Connection -> GroupsDTO -> Handler NoContent
createGroupHandler conn body = do
    res <- liftIO $ Group.insertGroup body conn
    case res of
        Left _ -> throwError err500
        Right [] -> throwError err403
        Right _ -> return NoContent

getGroupHandler :: Connection -> Text -> Handler [GroupsDTO]
getGroupHandler conn gid = do
    res <- liftIO $ Group.findGroup gid conn
    case res of
        Left _ -> throwError err500
        Right [] -> throwError err404
        Right as -> return $ map Group.toGroupDTO as

deleteGroupHandler :: Connection -> Text -> Handler NoContent
deleteGroupHandler conn gid = do
    res <- liftIO $ Group.deleteGroup gid conn
    case res of
        Left _ -> throwError err500
        Right [] -> throwError err404
        Right _ -> return NoContent

createPermissionHandler :: Connection -> UserPermissionsDTO -> Handler NoContent
createPermissionHandler conn body = do
    res <- liftIO $ UserPermissions.insertUserPermission body conn
    case res of
        Left _ -> throwError err500
        Right [] -> throwError err403
        Right _ -> return NoContent     

getPermissionHandler :: Connection -> Text -> Handler [UserPermissionsDTO]
getPermissionHandler conn rid = do
    res <- liftIO $ UserPermissions.findUserPermission rid conn
    case res of 
        Left _ -> throwError err500
        Right [] -> throwError err404
        Right as -> return $ map UserPermissions.toUserPermissionsDTO as     

deletePermissionHandler :: Connection -> Text -> Handler NoContent
deletePermissionHandler conn rid = do
    res <- liftIO $ UserPermissions.deleteUserPermission rid conn
    case res of
        Left _ -> throwError err500
        Right [] -> throwError err404
        Right _ -> return NoContent     

authorizeHandler :: Connection -> UserAuthorizationRequestDTO -> Handler UserAuthorizationDTO
authorizeHandler conn body = do
    res <- liftIO $ validateAuthorization body conn
    case res of
        Left _ -> throwError err500
        Right authRes -> return authRes  

createMapHandler :: Connection -> ResourceMapDTO -> Handler NoContent
createMapHandler conn body = do
    res <- liftIO $ ResourceMap.insertResourceMap body conn
    case res of     
        Left _ -> throwError err500
        Right [] -> throwError err403
        Right _ -> return NoContent 

getMapHandler :: Connection -> Text -> Handler ResourceMapDTO
getMapHandler conn rid = do 
    res <- liftIO $ getMap rid conn
    case res of
        Left _ -> throwError err500    
        Right Nothing -> throwError err404
        Right (Just m) -> return $ ResourceMap.toResourceMapDTO m 

deleteMapHandler :: Connection -> Text -> Handler NoContent
deleteMapHandler conn rid = do
    res <- liftIO $ ResourceMap.deleteResourceMap rid conn
    case res of
        Left _ -> throwError err500
        Right [] -> throwError err404
        Right _ -> return NoContent 

-- Helper functions
validateAuthorization :: UserAuthorizationRequestDTO -> Connection -> IO (Either QueryError UserAuthorizationDTO)
validateAuthorization body conn = do
    res <- UserPermissions.findUserAuthorization (authResource body) (authUserId body) conn
    case res of
        Left err -> return $ Left err
        Right [] -> return $ Left (error "No authorization found")
        Right (auth:_) -> return $ Right $ UserPermissions.toUserAuthorizationDTO auth

getMap :: Text -> Connection -> IO (Either QueryError (Maybe (ResourceMap.ResourceMap Result)))
getMap resource conn = do
    res <- ResourceMap.findResourceMap resource conn
    case res of
        Left err -> return $ Left err
        Right [] -> return $ Right Nothing
        Right (m:_) -> return $ Right (Just m)