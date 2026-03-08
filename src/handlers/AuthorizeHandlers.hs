{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module AuthorizeHandlers(loginHandler, authorizeHandler) where

import Servant
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Control.Monad.IO.Class (liftIO)
import Hasql.Connection (Connection)
import qualified Hasql.Pool as P
import Hasql.Pool (Pool)
import Hasql.Session (QueryError)
import UserPermissionsDTO (UserPermissionsDTO(..), UserAuthorizationRequestDTO(..), UserAuthorizationDTO(..)    )
import qualified UserPermissions
import qualified Tenant
import TokenModel (TokenResponse(..))
import qualified Token as Tkn
import Data.Time.Clock.POSIX (getPOSIXTime)
import TokenService (buildToken, toInt64, tokenExperitionTime, decodeToken)
import Network.Wai.Middleware.HttpAuth (extractBasicAuth, extractBearerAuth)

loginHandler :: Pool -> Maybe Text -> Handler TokenResponse
loginHandler pool mAuth = case mAuth of
    Nothing -> throwError err401
    Just auth -> case extractBasicAuth (TE.encodeUtf8 auth) of
        Nothing -> throwError err401
        Just (u, p) -> do
            let userText = TE.decodeUtf8 u
            let passText = TE.decodeUtf8 p
            res <- liftIO $ Tenant.findTenant userText passText pool
            case res of
                Left _ -> throwError err403
                Right [] -> throwError err403
                Right [t] -> do
                    cur <- liftIO getPOSIXTime
                    let expTime = toInt64 cur + 864000
                    let token = buildToken "client_credentials" (Tenant.getUserId t) expTime
                    _ <- liftIO $ Tkn.insertToken token "client_credentials" pool
                    return $ TokenResponse token "JWT" ""
    

authorizeHandler :: Pool -> UserAuthorizationRequestDTO -> Handler UserAuthorizationDTO
authorizeHandler pool body = do
    res <- liftIO $ validateAuthorization body pool
    case res of
        Left _ -> throwError err500
        Right authRes -> return authRes  


-- Helper functions
validateAuthorization :: UserAuthorizationRequestDTO -> Pool -> IO (Either P.UsageError UserAuthorizationDTO)
validateAuthorization body pool = do
    res <- UserPermissions.findUserAuthorization (authResource body) (authUserId body) pool
    case res of
        Left err -> return $ Left err
        Right [] -> return $ Left (error "No authorization found")
        Right (auth:_) -> return $ Right $ UserPermissions.toUserAuthorizationDTO auth