module StationCrawler.Queuers where

import StationCrawler.Types
import Types

import qualified Data.Map.Strict as M

queueTrains :: StationState -> TrainId -> StationState
queueTrains state trainId = if not . M.member trainId $ visited
  then
    state { 
      trainsLeftToVisit = trainId : (trainsLeftToVisit state) 
    , visitedTrains = M.insert trainId True visited
    }
  else
    state    
  where visited = visitedTrains state

queueStations :: StationState -> StationId -> StationState
queueStations state stationId = if not . M.member stationId $ visited
  then
    state { 
      stationsLeftToVisit = stationId : (stationsLeftToVisit state)
    , visitedStations = M.insert stationId True visited
    }
  else
    state
  where visited = visitedStations state

