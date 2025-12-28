{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}


module TokenModel where

import Data.Text 
import Data.Aeson
import GHC.Int

-- Token Response
data TokenResponse = TokenResponse
    { accessToken :: Text
    , tokenType :: Text
    , refreshToken :: Text
    } deriving (Show)

instance ToJSON TokenResponse where
    toJSON (TokenResponse accessToken tokenType refreshToken) = object
        [
            "accesstoken" .= accessToken,
            "tokentype" .= tokenType,
            "refreshtoken" .= refreshToken
        ]                           

-- Token Request
data TokenRequest = TokenRequest
    { clientid :: Text
    , clientsecret :: Text
    , granttype :: Text
    } deriving (Show)

instance ToJSON TokenRequest where
    toJSON (TokenRequest clientid clientsecret granttype) = object
        [
            "clientid" .= clientid,
            "clientsecret" .= clientsecret,
            "granttype" .= granttype
        ]

data Payload = Payload
    {
        aud :: Text,
        sub :: Text,
        exp :: Int64
    } deriving (Show)

instance ToJSON Payload where
    toJSON (Payload aud sub exp) = object
        [
            "aud" .= aud,
            "sub" .= sub,
            "exp" .= exp
        ] 

instance FromJSON Payload where
    parseJSON (Object v) = Payload <$>
        v .: "aud" <*>
        v .: "sub" <*>
        v .: "exp"                 