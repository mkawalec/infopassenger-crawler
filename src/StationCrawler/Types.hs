module StationCrawler.Types where

import qualified Data.Map.Strict as M
import Data.Default

import Types

data StationState = StationState {
  visitedStations :: M.Map StationId Bool
, visitedTrains :: M.Map TrainId Bool
, trainsLeftToVisit :: [TrainId]
, stationsLeftToVisit :: [StationId]
}

data UpdateType = TrainUpdate | StationUpdate

instance Default StationState where 
  def = StationState {
    visitedStations = M.empty
  , visitedTrains = M.empty
  , trainsLeftToVisit = []
  , stationsLeftToVisit = []
  }
