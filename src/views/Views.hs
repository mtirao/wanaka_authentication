module Views where

import Web.Scotty
import Data.Aeson(ToJSON)
import Web.Scotty.Internal.Types

jsonResponse :: ToJSON a => a -> ActionM ()
jsonResponse = json