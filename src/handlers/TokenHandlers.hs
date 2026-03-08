{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module TokenHandlers(tokenCreateHandler, tokenValidateHandler) where

import Servant
import Data.Text (Text)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Encoding as TE
import qualified Token as Tkn
import qualified Realm
import TokenService (buildToken, toInt64, tokenExperitionTime, decodeToken)
import TokenModel (TokenResponse(..))
import Hasql.Connection (Connection)
import qualified Hasql.Pool as P
import Hasql.Pool (Pool)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.Wai.Middleware.HttpAuth (extractBasicAuth, extractBearerAuth)

-- Handler for generating a token

tokenCreateHandler :: Pool -> Maybe Text -> Maybe Text -> Maybe Text -> Handler TokenResponse
tokenCreateHandler pool mClientId mClientSecret mGrant = case (mClientId, mClientSecret, mGrant) of
    (Just cid, Just csecret, Just _gtype) -> do
        res <- liftIO $ Realm.findRealm csecret pool
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
                    _ <- liftIO $ Tkn.insertToken token grantType pool
                    return $ TokenResponse token "JWT" ""
                else throwError err401
    _ -> throwError err400

tokenValidateHandler :: Pool -> Maybe Text -> Maybe Text -> Handler TokenResponse
tokenValidateHandler pool mAuth mClientId = case (mAuth, mClientId) of
    (Just auth, Just clientId) -> case extractBearerAuth (TE.encodeUtf8 auth) of
        Nothing -> throwError err401
        Just tok -> case decodeToken (TE.decodeUtf8 tok) of
            Nothing -> throwError err401
            Just payload -> do
                cur <- liftIO getPOSIXTime
                if tokenExperitionTime payload >= toInt64 cur then do
                    res <- liftIO $ Tkn.findToken (TE.decodeUtf8 tok) pool
                    case res of
                        Left _ -> throwError err500
                        Right [] -> throwError err401
                        Right [t] -> if Tkn.getClientId t == clientId then
                                        return $ TokenResponse (TE.decodeUtf8 tok) "JWT" ""
                                     else throwError err401
                else throwError err401
    _ -> throwError err400
