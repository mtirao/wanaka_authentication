{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}

module Tenant (findTenant, 
    insertTenant, 
    deleteTenant, 
    updatePassword, 
    getStatus, 
    getUserId, 
    getUserName) where

import Control.Monad.IO.Class
import Data.Int (Int32, Int64)
import Data.Text (Text, unpack)
import Data.Time (LocalTime)
import GHC.Generics (Generic)
import Hasql.Connection (Connection, ConnectionError, acquire, release, settings)
import Hasql.Session (QueryError, run, statement)
import Hasql.Statement (Statement (..))
import Rel8
import Prelude hiding (filter, null)

-- Rel8 Schemma Definitions
data Tenant f = Tenant
    { userName :: Column f Text
    , userPassword :: Column f Text
    , userId :: Column f Text
    , createdAt :: Column f Int64
    , status :: Column f Text
    }
    deriving (Generic, Rel8able)

deriving stock instance f ~ Rel8.Result => Show (Tenant f)

tenantSchema :: TableSchema (Tenant Name)
tenantSchema = TableSchema
    { name = "tenants"
    , schema = Nothing
    , columns = Tenant
        { userName = "user_name"
        , userPassword = "user_password"
        , userId = "user_id"
        , createdAt = "created_at"
        , status = "status"
        }
    }

findTenant :: Text -> Text -> Connection -> IO (Either QueryError [Tenant Result])
findTenant userName password conn =  do 
                            let query = select $ do
                                            p <- each tenantSchema
                                            where_ $ (p.userName ==. lit userName) &&. (p.userPassword ==. lit password)
                                            return p
                            run (statement () query ) conn

-- INSERT
insertTenant :: Text -> Text -> Text -> Text -> Int64-> Connection -> IO (Either QueryError [Text])
insertTenant u p r i c conn = do
                            run (statement () (insert1 u p r i c)) conn

insert1 :: Text -> Text -> Text -> Text -> Int64 -> Statement () [Text]
insert1 u p r i c = insert $ Insert 
            { into = tenantSchema
            , rows = values [ Tenant (lit u) (lit p) (lit i) (lit c) "new" ]
            , returning = Projection (.userId)
            , onConflict = Abort
            }

-- DELETE
deleteTenant :: Text -> Connection -> IO (Either QueryError [Text])
deleteTenant u conn = do
                        run (statement () (delete1 u )) conn

delete1 :: Text -> Statement () [Text]
delete1 u  = delete $ Delete
            { from = tenantSchema
            , using = pure ()
            , deleteWhere = \t ui -> ui.userId ==. lit u
            , returning = Projection (.userId)
            }

-- UPDATE
updatePassword :: Text -> Text -> Connection -> IO (Either QueryError [Text])
updatePassword u p conn = do
                        run (statement () (update1 u p)) conn

-- Update password
update1 :: Text -> Text -> Statement () [Text]
update1 u p  = update $ Update
            { target = tenantSchema
            , from = pure ()
            , set = \_ row -> Tenant row.userName (lit p) row.userId row.createdAt row.status
            , updateWhere = \t ui -> ui.userId ==. lit u
            , returning = Projection (.userId)
            }

-- Helper
getUserId :: Tenant Result -> Text
getUserId = (.userId)

getUserName :: Tenant Result -> Text
getUserName = (.userName)

getStatus :: Tenant Result -> Text
getStatus = (.status)