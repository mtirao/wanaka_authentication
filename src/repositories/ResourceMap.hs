{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}

module ResourceMap  where

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

import ResourceMapDTO
import Group (getGroupId)

-- Rel8 Schemma Definitions
data ResourceMap f = ResourceMap
    { resMapUserId :: Column f Text
    , resMapGroupId :: Column f Text
    , resMapResource :: Column f Text
    }
    deriving (Generic, Rel8able)

deriving stock instance f ~ Rel8.Result => Show (ResourceMap f)

resourceMapSchema :: TableSchema (ResourceMap Name)
resourceMapSchema = TableSchema
    { name = "resource_mappings"
    , schema = Nothing
    , columns = ResourceMap
        { resMapUserId = "user_id"
        , resMapGroupId = "group_id"
        , resMapResource = "resource"
        }
    }

-- Functions
-- GET
findResourceMap :: Text -> Pool -> IO (Either P.UsageError [ResourceMap Result])
findResourceMap resource pool = do
                            let query = select $ do
                                            p <- each resourceMapSchema
                                            where_ $ p.resMapResource ==. lit resource
                                            return p
                            P.use pool (statement () query)

-- INSERT
insertResourceMap :: ResourceMapDTO -> Pool -> IO (Either P.UsageError [Text])
insertResourceMap p pool = do
                            P.use pool (statement () (insert1 p))

insert1 :: ResourceMapDTO -> Statement () [Text]
insert1 p = insert $ Insert
            { into = resourceMapSchema
            , rows = values [ ResourceMap (lit p.resMapUserId) (lit p.resMapGroupId) (lit p.resMapResource)]
            , returning = Projection (.resMapResource)
            , onConflict = Abort
            }

-- DELETE
deleteResourceMap :: Text -> Pool -> IO (Either P.UsageError [Text])
deleteResourceMap u pool = do
                        P.use pool (statement () (delete1 u ))

delete1 :: Text -> Statement () [Text]
delete1 u  = delete $ Delete
            { from = resourceMapSchema
            , using = pure ()
            , deleteWhere = \t ui -> ui.resMapResource ==. lit u
            , returning = Projection (.resMapResource)
            }

-- UPDATE
updateResourceMap :: Text -> ResourceMapDTO -> Pool -> IO (Either P.UsageError [Text])
updateResourceMap u p pool = do
                        P.use pool (statement () (update1 u p))

update1 :: Text -> ResourceMapDTO -> Statement () [Text]
update1 u p  = update $ Update
            { target = resourceMapSchema
            , from = pure ()
            , set = \_ row -> ResourceMap (lit p.resMapUserId) (lit p.resMapGroupId) (lit p.resMapResource)
            , updateWhere = \t ui -> ui.resMapUserId ==. lit u
            , returning = Projection (.resMapUserId)
            }

-- Helpers
toResourceMapDTO :: ResourceMap Result -> ResourceMapDTO
toResourceMapDTO p = ResourceMapDTO p.resMapUserId  p.resMapGroupId  p.resMapResource

getResUserId :: ResourceMap Result -> Text
getResUserId p = p.resMapUserId

getResGroupId :: ResourceMap Result -> Text
getResGroupId p = p.resMapGroupId