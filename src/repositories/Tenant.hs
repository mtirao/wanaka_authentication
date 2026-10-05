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
import qualified Hasql.Session as Session
import qualified Hasql.Pool as P
import Hasql.Pool (Pool)
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
    , columns = Tenant
        { userName = "user_name"
        , userPassword = "user_password"
        , userId = "user_id"
        , createdAt = "created_at"
        , status = "status"
        }
    }

findTenant :: Text -> Text -> Pool -> IO (Either P.UsageError [Tenant Result])
findTenant userName password pool =  do 
                            let query = select $ do
                                            p <- each tenantSchema
                                            where_ $ (p.userName ==. lit userName) &&. (p.userPassword ==. lit password)
                                            return p
                            P.use pool (Session.statement () (run query))

-- INSERT
insertTenant :: Text -> Text -> Text -> Text -> Int64-> Pool -> IO (Either P.UsageError [Text])
insertTenant u p r i c pool = do
                            P.use pool (Session.statement () (run (insert1 u p r i c)))

insert1 :: Text -> Text -> Text -> Text -> Int64 -> Statement (Query (Expr Text))
insert1 u p r i c = insert $ Insert 
            { into = tenantSchema
            , rows = values [ Tenant (lit u) (lit p) (lit i) (lit c) "new" ]
            , returning = Returning (.userId)
            , onConflict = Abort
            }

-- DELETE
deleteTenant :: Text -> Pool -> IO (Either P.UsageError [Text])
deleteTenant u pool = do
                        P.use pool (Session.statement () (run (delete1 u)))

delete1 :: Text -> Statement (Query (Expr Text))
delete1 u  = delete $ Delete
            { from = tenantSchema
            , using = pure ()
            , deleteWhere = \t ui -> ui.userId ==. lit u
            , returning = Returning (.userId)
            }

-- UPDATE
updatePassword :: Text -> Text -> Pool -> IO (Either P.UsageError [Text])
updatePassword u p pool = do
                        P.use pool (Session.statement () (run (update1 u p)))

-- Update password
update1 :: Text -> Text -> Statement (Query (Expr Text))
update1 u p  = update $ Update
            { target = tenantSchema
            , from = pure ()
            , set = \_ row -> Tenant row.userName (lit p) row.userId row.createdAt row.status
            , updateWhere = \t ui -> ui.userId ==. lit u
            , returning = Returning (.userId)
            }

-- Helper
getUserId :: Tenant Result -> Text
getUserId = (.userId)

getUserName :: Tenant Result -> Text
getUserName = (.userName)

getStatus :: Tenant Result -> Text
getStatus = (.status)