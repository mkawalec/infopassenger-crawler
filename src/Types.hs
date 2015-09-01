module Types where

import Data.Time.LocalTime (LocalTime)
import Data.Time.Format (readTime, defaultTimeLocale)

data Connection = Connection {
  connId :: !Integer
, trainId :: !String
, arrivalTime :: !(Maybe LocalTime)
, delayTime :: !Integer
} deriving (Show, Read, Ord, Eq)

data Station = Station {
  stationId :: Integer
, stationName :: String
, connections :: ![Connection]
} deriving (Show, Read, Ord, Eq)

data RequestType = StationRequest | TrainRequest
  deriving (Show, Eq)

type TrainId = Integer
type StationId = Integer
