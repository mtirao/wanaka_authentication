{-# language BlockArguments #-}
{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DerivingVia #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedStrings #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}

module Token(findToken, insertToken, deleteToken, updateToken, getClientId) where

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

data Token f = Token
    {authtoken :: Column f Text
    , clientid :: Column f Text
    }
    deriving (Generic, Rel8able)

deriving stock instance f ~ Rel8.Result => Show (Token f)

tokenSchema :: TableSchema (Token Name)
tokenSchema = TableSchema
    { name = "tokens"
    , schema = Nothing
    , columns = Token
        { authtoken = "auth_token"
        , clientid = "client_id"
        }
    }

--Function
-- SELECT
findToken :: Text -> Connection -> IO (Either QueryError [Token Result])
findToken token conn = do
                            let query = select $ do
                                            p <- each tokenSchema
                                            where_ $ (p.authtoken ==. lit token)
                                            return p
                            run (statement () query ) conn

-- INSERT
insertToken :: Text -> Text -> Connection -> IO (Either QueryError [Text])
insertToken a c  conn = do
                            run (statement () (insert1 a c)) conn

insert1 ::  Text -> Text -> Statement () [Text]
insert1 a c = insert $ Insert
            { into = tokenSchema
            , rows = values [ Token (lit a) (lit c) ]
            , returning = Projection (.clientid)
            , onConflict = Abort
            }

-- UPDATE
updateToken :: Text -> Text -> Connection -> IO (Either QueryError [Text])
updateToken t p conn = do
                        run (statement () (update1 t p)) conn

update1 :: Text -> Text -> Statement () [Text]
update1 t u  = update $ Update
            { target = tokenSchema
            , from = pure ()
            , set = \_ row -> Token (lit t) (lit u)
            , updateWhere = \t ui -> ui.clientid ==. lit u
            , returning = Projection (.clientid)
            }

-- DELETE
deleteToken :: Text -> Connection -> IO (Either QueryError [Text])
deleteToken u conn = do
                        run (statement () (delete1 u )) conn

delete1 :: Text -> Statement () [Text]
delete1 u  = delete $ Delete
            { from = tokenSchema
            , using = pure ()
            , deleteWhere = \t ui -> ui.authtoken ==. lit u
            , returning = Projection (.clientid)
            }

getClientId :: Token Result -> Text
getClientId r = r.clientid