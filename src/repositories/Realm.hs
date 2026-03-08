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
import qualified Hasql.Pool as P
import Hasql.Pool (Pool)
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
findRealm :: Text -> Pool -> IO (Either P.UsageError [Realm Result])
findRealm clientsecret pool = do
                            let query = select $ do
                                            p <- each realmSchema
                                            where_ $ p.clientsecret ==. lit clientsecret
                                            return p
                            P.use pool (statement () query)

-- INSERT
insertRealm :: TokenRequest -> Pool -> IO (Either P.UsageError [Text])
insertRealm p pool = do
                            P.use pool (statement () (insert1 p))

insert1 :: TokenRequest -> Statement () [Text]
insert1 p = insert $ Insert
            { into = realmSchema
            , rows = values [ Realm (lit $ p.clientid) (lit $ p.clientsecret) (lit $ p.granttype) ]
            , returning = Projection (.clientid)
            , onConflict = Abort
            }

-- DELETE
deleteRealm :: Text -> Pool -> IO (Either P.UsageError [Text])
deleteRealm u pool = do
                        P.use pool (statement () (delete1 u ))

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
