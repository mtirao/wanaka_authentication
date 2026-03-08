{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Server (app) where

import Servant
import Network.Wai (Application, Middleware, Request(..), responseLBS, getRequestBodyChunk)
import Network.HTTP.Types (status401)
import Data.Text (Text)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString as B
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Text.Encoding as TE
import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.Wai.Middleware.HttpAuth (extractBasicAuth, extractBearerAuth)

import Hasql.Connection (Connection)
import Hasql.Session (QueryError, Session, run)
import qualified Hasql.Pool as P
import Hasql.Pool (Pool)

import qualified Token as Tkn
import qualified Tenant
import qualified UserPermissions
import TokenService (buildToken, toInt64, tokenExperitionTime, decodeToken)
import TokenModel (TokenResponse(..))
import GroupsDTO (GroupsDTO(..))
import UserPermissionsDTO (UserPermissionsDTO(..), UserAuthorizationRequestDTO(..), UserAuthorizationDTO(..)    )
import ResourceMapDTO (ResourceMapDTO(..))

-- Handlers
import TokenHandlers (tokenCreateHandler, tokenValidateHandler)
import GroupHandlers (createGroupHandler, getGroupHandler, deleteGroupHandler)
import PermissionHandlers (createPermissionHandler, getPermissionHandler, deletePermissionHandler)
import MapHandlers (createMapHandler, getMapHandler, deleteMapHandler)
import AuthorizeHandlers (loginHandler, authorizeHandler)

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
app :: Pool -> Application
app pool = loggingMiddleware $ tokenMiddleware pool $ serve api (server pool)

-- Server implementation
server :: Pool -> Server API
server pool = loginHandler pool
         :<|> tokenCreateHandler pool
         :<|> tokenValidateHandler pool
         :<|> createGroupHandler pool
         :<|> getGroupHandler pool
         :<|> deleteGroupHandler pool
         :<|> createPermissionHandler pool
         :<|> getPermissionHandler pool
         :<|> deletePermissionHandler pool
         :<|> authorizeHandler pool
         :<|> createMapHandler pool
         :<|> getMapHandler pool
         :<|> deleteMapHandler pool 


-- Middleware and helpers

-- | Simple token check using same logic as tokenValidateHandler (minus clientId)
checkToken :: Pool -> Text -> IO Bool
checkToken pool t = case decodeToken t of
    Nothing -> return False
    Just payload -> do
        cur <- liftIO getPOSIXTime
        if tokenExperitionTime payload >= toInt64 cur then do
            res <- Tkn.findToken t pool
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


tokenMiddleware :: Pool -> Middleware
tokenMiddleware pool app req respond =
    if skipAuth (pathInfo req) then
        app req respond
    else case lookup "authorization" (requestHeaders req) of
        Nothing -> respond $ responseLBS status401 [("Content-Type","text/plain")] "Unauthorized"
        Just auth -> case extractBearerAuth auth of
            Nothing -> respond $ responseLBS status401 [("Content-Type","text/plain")] "Unauthorized"
            Just tok -> do
                let tokText = TE.decodeUtf8 tok
                valid <- checkToken pool tokText
                if valid then app req respond
                         else respond $ responseLBS status401 [("Content-Type","text/plain")] "Unauthorized"



