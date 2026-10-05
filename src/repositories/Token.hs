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
import qualified Hasql.Session as Session
import qualified Hasql.Pool as P
import Hasql.Pool (Pool)
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
    , columns = Token
        { authtoken = "auth_token"
        , clientid = "client_id"
        }
    }

--Function
-- SELECT
findToken :: Text -> Pool -> IO (Either P.UsageError [Token Result])
findToken token pool = do
                            let query = select $ do
                                            p <- each tokenSchema
                                            where_ $ (p.authtoken ==. lit token)
                                            return p
                            P.use pool (Session.statement () (run query))

-- INSERT
insertToken :: Text -> Text -> Pool -> IO (Either P.UsageError [Text])
insertToken a c pool = do
                            P.use pool (Session.statement () (run (insert1 a c)))

insert1 :: Text -> Text -> Statement (Query (Expr Text))
insert1 a c = insert $ Insert
            { into = tokenSchema
            , rows = values [ Token (lit a) (lit c) ]
            , returning = Returning (.clientid)
            , onConflict = Abort
            }

-- UPDATE
updateToken :: Text -> Text -> Pool -> IO (Either P.UsageError [Text])
updateToken t p pool = do
                        P.use pool (Session.statement () (run (update1 t p)))

update1 :: Text -> Text -> Statement (Query (Expr Text))
update1 t u  = update $ Update
            { target = tokenSchema
            , from = pure ()
            , set = \_ row -> Token (lit t) (lit u)
            , updateWhere = \t ui -> ui.clientid ==. lit u
            , returning = Returning (.clientid)
            }

-- DELETE
deleteToken :: Text -> Pool -> IO (Either P.UsageError [Text])
deleteToken u pool = do
                        P.use pool (Session.statement () (run (delete1 u)))

delete1 :: Text -> Statement (Query (Expr Text))
delete1 u  = delete $ Delete
            { from = tokenSchema
            , using = pure ()
            , deleteWhere = \t ui -> ui.authtoken ==. lit u
            , returning = Returning (.clientid)
            }

getClientId :: Token Result -> Text
getClientId r = r.clientid