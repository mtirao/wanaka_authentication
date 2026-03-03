{-# LANGUAGE OverloadedStrings #-}

module TokenService where

import Data.Time
import Data.Aeson (FromJSON, ToJSON, encode, decode)
import Data.Time.Clock.POSIX
import Data.Text (Text, unpack, pack)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Lazy.Internal as BL
import qualified Data.ByteString.Internal as BI
import qualified Data.Text.Encoding as T

import Control.Monad.IO.Class

import GHC.Int

import Jose.Jws
import Jose.Jwa
import Jose.Jwt (Jwt(Jwt), JwsHeader(JwsHeader))
import Network.Wai.Middleware.HttpAuth

import ErrorMessage
import qualified Token as Tkn
import Realm
import TokenModel
import Control.Monad.Trans.Class (MonadTrans(lift))


-- createToken conn clientid granttype= do
--        curTime <- liftIO getPOSIXTime
--        let expDate = tokenExpiration curTime
--        let token = buildToken granttype clientid expDate
--        liftIO $ Tkn.insertToken token granttype conn
--        jsonResponse (TokenResponse token "JWT" "" )


-- Token helpers
convertToString :: Text -> Text -> Int64 -> [Char]
convertToString aud sub t = BL.unpackChars (encode $ Payload aud sub t)

buildToken :: Text -> Text -> Int64 -> Text
buildToken aud sub t = case token of
                        Left _ -> ""
                        Right (Jwt jwt) -> T.decodeUtf8 jwt
                    where
                        payload = BI.packChars $ convertToString aud sub t
                        token = hmacEncode HS256 "xqKaj2OIBO2gPHwa6miCVR8z00Wbwsn5Se8xyN0F6T0" payload

decodeToken :: Text -> Maybe Payload
decodeToken t = case token of
                    Left _ -> Nothing
                    Right (_, jwt) -> convertToPayload jwt
                where
                    token = hmacDecode "xqKaj2OIBO2gPHwa6miCVR8z00Wbwsn5Se8xyN0F6T0" $ tokenFromHeader (TL.fromStrict t)
                    tokenFromHeader t = BL.toStrict $ TL.encodeUtf8 t


--- Helper Functions
convertToPayload :: BI.ByteString -> Maybe Payload
convertToPayload t = ( decode $  BL.packChars $ BI.unpackChars t ) :: Maybe Payload

tokenExpiration :: NominalDiffTime -> Int64
tokenExpiration u = toInt64 u + 864000

toInt64 :: NominalDiffTime -> Int64
toInt64 = floor . nominalDiffTimeToSeconds

tokenExperitionTime :: Payload -> Int64
tokenExperitionTime (Payload a s e) = e