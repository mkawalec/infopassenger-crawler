module MergeDelays(computeCurrentDelays) where

import Types
import qualified Data.Map.Strict as M

type StationState = M.Map Integer Station
type ConnState = M.Map Integer Connection

mergeConnection :: ConnState -> Connection -> Connection
mergeConnection connCache connection = connection { delayTime = maximum delays }
  where oldConnection = M.lookup (trainId connection) connCache
        oldDelay = case oldConnection of
          Just conn -> delayTime conn
          Nothing -> 0
        delays = [oldDelay, delayTime connection]


mergeConnections :: Station -> ConnState -> [Connection]
mergeConnections station connCache = map (mergeConnection connCache) conns
  where conns = connections station

reduceStation :: StationState -> Station -> StationState
reduceStation state station = M.insert (stationId station) newStationEntry state
  where currentStation = M.lookup (stationId station) state
        unpackConnections = (\station -> map (\conn -> (trainId conn, conn)) (connections station))
        connCache = case currentStation of
          Just currStation -> M.fromList $ unpackConnections currStation
          Nothing -> M.empty
        newStationEntry = station { connections = mergeConnections station connCache }

computeCurrentDelays :: StationState -> [Station] -> M.Map Integer Station
computeCurrentDelays state stations = foldl reduceStation state stations

