module ErrorMessage where

import Data.Text 
import Data.Aeson

--ErrorMessage
newtype ErrorMessage = ErrorMessage Text
    deriving (Show)

instance ToJSON ErrorMessage where
    toJSON (ErrorMessage message) = object
        [
            "error" .= message
        ]
