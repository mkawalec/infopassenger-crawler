module Types where

import Data.Time.LocalTime (LocalTime)
import qualified Data.Map.Strict as M
import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.FromRow (field, fromRow)

data Connection = Connection {
  connId :: !Integer
, trainId :: !String
, arrivalTime :: !(Maybe LocalTime)
, delayTime :: !Integer
} deriving (Show, Read, Ord, Eq)

data Station = Station {
  stationId :: !Integer
, stationName :: !String
, connections :: ![Connection]
} deriving (Show, Read, Ord, Eq)

data DbStation = DbStation {
  dbStationDbId :: Integer
, dbStationName :: String
, dbStationId :: Integer
} deriving (Show, Ord, Eq)

instance FromRow DbStation where
  fromRow = DbStation <$> field <*> field <*> field

data RequestType = StationRequest | TrainRequest
  deriving (Show, Eq)

type TrainId = Integer
type StationId = Integer

type StationCache = M.Map String Station
type ConnCache = M.Map Integer Connection
