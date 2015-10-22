module DbOpts.UpdatesSolver (
  getCommonStations, 
  getUpdatedConnections, 
  getAddedStations
  ) where

import DbOpts.Types
import Types
import DbOpts.ConnToDb

import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Debug.Trace as DT

getCommonStations :: StationCache -> StationCache -> [(Station,Station)]
getCommonStations oldState newState = zip (getStations oldState) (getStations newState)
  where newIds           = map fst $ M.toList newState
        doExist          = map (flip M.member oldState) newIds
        commonStationIds = map snd $ filter fst $ zip doExist newIds
        getStations = (\state -> map (state M.!) commonStationIds)

isConnectionAdded :: M.Map Integer Connection -> Connection -> Bool
isConnectionAdded oldConnsCache connection = if isInCache
  then
    (arrivalTime connection) == (arrivalTime oldConnection)
  else
    False
  where isInCache = M.member (connId connection) oldConnsCache
        oldConnection = oldConnsCache M.! (connId connection)
        

getUpdatedConnections' :: 
  (M.Map String Integer) ->
  ([DbConnection],[(Connection, String)]) -> 
  (Station,Station) -> 
  ([DbConnection],[(Connection, String)])
getUpdatedConnections' stationCache (currAdded, currUpd) (oldStation,newStation) = 
  (addedDbConns ++ currAdded, updatedConnsWithStation ++ currUpd)
  where oldConnsCache = M.fromList $ map (\conn -> (connId conn, conn)) (connections oldStation)
        newConns      = connections newStation

        (existingConns, addedConns) = L.partition (isConnectionAdded oldConnsCache) newConns
        getValues                   = (\x -> map snd (M.toList x))
        currStationId               = stationCache M.! (stationName newStation)

        addedDbConns = map (connToDbConn currStationId) addedConns
        updatedConns = filter (\conn -> (oldConnsCache M.! (connId conn)) /= conn) existingConns
        updatedConnsWithStation = map (\conn -> (conn, stationName newStation)) updatedConns

getUpdatedConnections 
  :: M.Map String Integer -> 
     [(Station,Station)] -> 
     ([DbConnection], [(Connection, String)])
getUpdatedConnections stationCache stations = foldl (getUpdatedConnections' stationCache) ([], []) stations
  
getAddedStations :: StationCache -> StationCache -> [Station]
getAddedStations oldState newState = map (newState M.!) addedNames
  where newNames   = map fst $ M.toList newState
        doExist    = map (flip M.member oldState) newNames
        addedNames = map snd $ filter (not . fst) $ zip doExist newNames
