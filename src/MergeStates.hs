module MergeStates where

import Types
import qualified Data.Map.Strict as M
import qualified Debug.Trace as DT

-- We need to remember old stations (but not old connections)
mergeStates :: StationCache -> StationCache -> StationCache
mergeStates oldState newState = withOldStations
  where oldStations = map snd $ M.toList oldState
        withOldStations = foldl (\acc station -> case M.member (stationName station) acc of
          True -> newState
          False -> M.insert (stationName station) (station { connections = [] }) acc
          ) newState oldStations
