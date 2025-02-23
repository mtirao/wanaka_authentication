{-# LANGUAGE OverloadedStrings #-}

module AuthenticationController(userAuthenticate) where

import Web.Scotty ( body, header, status, ActionM )
import Web.Scotty.Internal.Types (ActionT)
import Web.Scotty.Trans (ScottyT, get, json, put)
import Network.Wai
import Network.Wai.Middleware.HttpAuth
import Network.HTTP.Types.Status
import Data.Time
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import Data.Time.Clock.POSIX
import Data.Text (Text, unpack, pack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Lazy.Internal as BL
import qualified Data.ByteString.Internal as BI
import qualified Data.Text.Encoding as T

import GHC.Int

import Control.Monad.IO.Class
import TenantsModel
import Tenant
import Views
import ErrorMessage

userAuthenticate conn =  do
        h <- header "Authorization"
        case h >>= extractBasicAuth . T.encodeUtf8 . TL.toStrict of
            Just (user, password) -> do
                let userText = T.decodeUtf8 user
                let passwordText = T.decodeUtf8 password
                result <- liftIO $ findTenant userText passwordText conn
                case result of
                    Left e -> do
                            liftIO $ print result
                            jsonResponse (ErrorMessage "User not found")
                            status forbidden403
                    Right [] -> do
                        jsonResponse (ErrorMessage "User not found")
                        status forbidden403
                    Right [a] -> do
                            jsonResponse (TenantResponse (TL.fromStrict $ getUserName a) (TL.fromStrict $ getUserId a) (TL.fromStrict $ getStatus a))
                            status ok200
            Nothing -> status unauthorized401