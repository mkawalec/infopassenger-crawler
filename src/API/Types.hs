module API.Types where

import GHC.Generics
import Data.Time.LocalTime (LocalTime(..))
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow (field, fromRow)
import Data.Aeson (ToJSON)
  
data APIConnection = APIConnection {
  connDelay' :: !Integer
, connId' :: !Integer
, trainId' :: !String
, arrivalTime' :: !(Maybe LocalTime)
, stationName' :: String
} deriving (Show, Generic)

instance FromRow APIConnection where
  fromRow = APIConnection <$> field <*> field <*> field <*> field <*> field

instance ToJSON APIConnection

