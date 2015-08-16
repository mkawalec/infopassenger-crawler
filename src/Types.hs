module Types where

import Data.Time.LocalTime (LocalTime)

data Connection = Connection {
  trainId :: Integer
, arrivalTime :: LocalTime
, delayTime :: Integer
} deriving (Show, Ord, Eq)

data Station = Station {
  stationId :: Integer
, stationName :: String
, connections :: ![Connection]
} deriving (Show, Ord, Eq)

data RequestType = StationRequest | TrainRequest
  deriving (Show, Eq)

type TrainId = Integer
type StationId = Integer
