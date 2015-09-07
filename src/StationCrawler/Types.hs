module StationCrawler.Types where

import qualified Data.Map as M
import Data.Default

import Types

data StationState = StationState {
  queuedStations :: M.Map StationId Bool
, queuedTrains :: M.Map TrainId Bool
, trainsLeftToVisit :: [TrainId]
, stationsLeftToVisit :: [StationId]
}

data UpdateType = TrainUpdate | StationUpdate

instance Default StationState where 
  def = StationState {
    queuedStations = M.empty
  , queuedTrains = M.empty
  , trainsLeftToVisit = []
  , stationsLeftToVisit = []
  }
