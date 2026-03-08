{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}

module Group (findGroup, toGroupDTO, insertGroup, deleteGroup, updateGroup, getGroupId) where

import Control.Monad.IO.Class
import Data.Int (Int32, Int64)
import Data.Text (Text, unpack, pack)
import qualified Data.Text.Lazy as TL
--import qualified Data.Text.Internal as TI
import Data.Time (LocalTime)
import GHC.Generics (Generic)
import Hasql.Connection (Connection, ConnectionError, acquire, release, settings)
import Hasql.Session (QueryError, run, statement)
import Hasql.Statement (Statement (..))
import qualified Hasql.Pool as P
import Hasql.Pool (Pool)
import Rel8
import Prelude hiding (filter, null)

import GroupsDTO

-- Rel8 Schemma Definitions
data Group f = Group
    {userId  :: Column f Text
    , groupId :: Column f Text
    }
    deriving (Generic, Rel8able)

deriving stock instance f ~ Rel8.Result => Show (Group f)

groupSchema :: TableSchema (Group Name)
groupSchema = TableSchema
    { name = "groups"
    , schema = Nothing
    , columns = Group
        { userId = "user_id"
        , groupId = "group_id"
        }
    }

-- Functions
-- GET
findGroup :: Text -> Pool -> IO (Either P.UsageError [Group Result])
findGroup userId pool = do
                            let query = select $ do
                                            p <- each groupSchema
                                            where_  (p.userId ==. lit userId)
                                            return p
                            P.use pool (statement () query)

-- INSERT
insertGroup :: GroupsDTO -> Pool -> IO (Either P.UsageError [Text])
insertGroup p pool = do
                            P.use pool (statement () (insert1 p))

insert1 :: GroupsDTO -> Statement () [Text]
insert1 p = insert $ Insert
            { into = groupSchema
            , rows = values [ Group (lit p.groupUserId) (lit p.groupId)]
            , returning = Projection (.userId)
            , onConflict = Abort
            }

-- DELETE
deleteGroup :: Text -> Pool -> IO (Either P.UsageError [Text])
deleteGroup u pool = do
                        P.use pool (statement () (delete1 u ))

delete1 :: Text -> Statement () [Text]
delete1 u  = delete $ Delete
            { from = groupSchema
            , using = pure ()
            , deleteWhere = \t ui -> ui.userId ==. lit u
            , returning = Projection (.userId)
            }

-- UPDATE
updateGroup :: Text -> GroupsDTO -> Pool -> IO (Either P.UsageError [Text])
updateGroup u p pool = do
                        P.use pool (statement () (update1 u p))

update1 :: Text -> GroupsDTO -> Statement () [Text]
update1 u p  = update $ Update
            { target = groupSchema
            , from = pure ()
            , set = \_ row -> Group (lit p.groupUserId) (lit p.groupId)
            , updateWhere = \t ui -> ui.userId ==. lit u
            , returning = Projection (.userId)
            }

-- Helpers
toGroupDTO :: Group Result -> GroupsDTO
toGroupDTO p = GroupsDTO p.userId p.groupId

getGroupId :: Group Result -> Text
getGroupId p = p.groupId
