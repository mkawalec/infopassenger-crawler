module DbOpts.UpdatesSolver where

import DbOpts.Types
import Types
import DbOpts.ConnToDb

import qualified Data.Map.Strict as M
import qualified Data.List as L

getCommonStations :: StationCache -> StationCache -> [(Station,Station)]
getCommonStations oldState newState = zip (getStations oldState) (getStations newState)
  where newIds = map fst $ M.toList newState
        doExist = map (flip M.member oldState) newIds
        commonStationIds = map snd $ filter fst $ zip doExist newIds
        getStations = (\state -> map (state M.!) commonStationIds)

getUpdatedConnections' :: 
  (M.Map String Integer) ->
  ([DbConnection],[(Connection, String)]) -> 
  (Station,Station) -> 
  ([DbConnection],[(Connection, String)])
getUpdatedConnections' stationCache (currAdded, currUpd) (oldStation,newStation) = 
  (addedDbConns ++ currAdded, updatedConnsWithStation ++ currUpd)
  where oldConns = map (\conn -> (connId conn, conn)) (connections oldStation)
        oldConnsCache = M.fromList oldConns
        newConns = connections newStation
        (existingConns, addedConns) = L.partition (\conn -> M.member (connId conn) oldConnsCache) newConns
        getValues = (\x -> map snd (M.toList x))
        currStationId = stationCache M.! (stationName newStation)

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
  where newNames = map fst $ M.toList newState
        doExist = map (flip M.member oldState) newNames
        addedNames = map snd $ filter (not . fst) $ zip doExist newNames
