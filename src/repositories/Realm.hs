{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}

module Realm(findRealm, 
    insertRealm, 
    deleteRealm, 
    getClientId, 
    getClientSecret, 
    getGrantType) where

import Control.Monad.IO.Class
import Data.Int (Int32, Int64)
import Data.Text (Text, unpack, pack)
import Data.Time (LocalTime)
import GHC.Generics (Generic)
import Hasql.Connection (Connection, ConnectionError, acquire, release, settings)
import Hasql.Session (QueryError, run, statement)
import Hasql.Statement (Statement (..))
import Rel8
import Prelude hiding (filter, null)
import TokenModel
import Control.Monad.Trans.RWS (get)

data Realm f = Realm
    {clientid :: Column f Text
    , clientsecret :: Column f Text
    , granttype :: Column f Text
    }
    deriving (Generic, Rel8able)

deriving stock instance f ~ Rel8.Result => Show (Realm f)

realmSchema :: TableSchema (Realm Name)
realmSchema = TableSchema
    { name = "realms"
    , schema = Nothing
    , columns = Realm
        { clientid = "client_id"
        , clientsecret = "client_secret"
        , granttype= "grant_type"
        }
    }

--Function
-- SELECT
findRealm :: Text -> Connection -> IO (Either QueryError [Realm Result])
findRealm clientsecret conn = do
                            let query = select $ do
                                            p <- each realmSchema
                                            where_ $ p.clientsecret ==. lit clientsecret
                                            return p
                            run (statement () query ) conn

-- INSERT
insertRealm :: TokenRequest -> Connection -> IO (Either QueryError [Text])
insertRealm p  conn = do
                            run (statement () (insert1 p)) conn

insert1 :: TokenRequest -> Statement () [Text]
insert1 p = insert $ Insert
            { into = realmSchema
            , rows = values [ Realm (lit $ p.clientid) (lit $ p.clientsecret) (lit $ p.granttype) ]
            , returning = Projection (.clientid)
            , onConflict = Abort
            }

-- DELETE
deleteRealm :: Text -> Connection -> IO (Either QueryError [Text])
deleteRealm u conn = do
                        run (statement () (delete1 u )) conn

delete1 :: Text -> Statement () [Text]
delete1 u  = delete $ Delete
            { from = realmSchema
            , using = pure ()
            , deleteWhere = \t ui -> (ui.clientsecret ==. lit u)
            , returning = Projection (.clientsecret)
            }

getClientId :: Realm Result -> Text
getClientId r = r.clientid

getClientSecret :: Realm Result -> Text
getClientSecret r = r.clientsecret

getGrantType :: Realm Result -> Text
getGrantType r = r.granttype
