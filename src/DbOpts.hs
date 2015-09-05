module DbOpts(persistDelays) where

import Types hiding (Connection)
import qualified Types as T
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Database.PostgreSQL.Simple
import GHC.Int (Int64)
import qualified Debug.Trace as DT
import Control.Monad
import Data.Time.LocalTime (LocalTime)

connectionData = defaultConnectInfo {
  connectPort = 2345
, connectUser = "info"
, connectDatabase = "info"
, connectPassword = "info"
}

connInsertQuery = "insert into connections (delay, conn_id, train_id, arrival_time, station_id) values (?, ?, ?, ?, ?)"
connUpdateQuery = "UPDATE connections SET delay = ?, arrival_time = ? WHERE conn_id = ? AND station_id = ?"

getStations :: Connection -> IO [DbStation]
getStations c = query_ c "SELECT id, name, station_id FROM stations"

-- Connection data in a db-ready form
type DbConnection = (Integer, Integer, String, Maybe LocalTime, Integer)

getAddedStations :: StationCache -> StationCache -> [Station]
getAddedStations oldState newState = map (newState M.!) addedNames
  where newNames = map fst $ M.toList newState
        doExist = map (flip M.member oldState) newNames
        addedNames = map snd $ filter (not . fst) $ zip doExist newNames

mapConnections :: M.Map String Integer -> [DbConnection] -> Station -> [DbConnection]
mapConnections stationCache acc station = dbConns ++ acc
  where conns = connections station
        stationDbId = stationCache M.! (stationName station)
        dbConns = map (connToDbConn stationDbId) conns

connToDbConn :: Integer -> T.Connection -> DbConnection
connToDbConn stationDbId x = (delayTime x, connId x, trainId x, arrivalTime x, stationDbId)

getConnections :: [Station] -> [DbStation] -> ([DbConnection], M.Map String Integer)
getConnections stations dbStations = (foldl (mapConnections nameCache) [] stations, nameCache)
  where namePairs = map (\x -> (dbStationName x, dbStationDbId x)) dbStations
        nameCache = M.fromList namePairs

addStations :: Connection -> [Station] -> IO (M.Map String Integer)
addStations connection stations = do
  let usefulStations = filter (\x -> (length . stationName $ x) > 0) stations
  let stationData = map (\s -> (stationId s, stationName s)) usefulStations

  executeMany connection "insert into stations (station_id, name) values (?, ?)" stationData

  -- We need to get the current stations data
  dbStations <- getStations connection
  let (trainConnections,nameCache) = getConnections usefulStations dbStations

  executeMany connection connInsertQuery trainConnections
  return nameCache

getCommonStations :: StationCache -> StationCache -> [(Station,Station)]
getCommonStations oldState newState = zip (getStations oldState) (getStations newState)
  where newIds = map fst $ M.toList newState
        doExist = map (flip M.member oldState) newIds
        commonStationIds = map snd $ filter fst $ zip doExist newIds
        getStations = (\state -> map (state M.!) commonStationIds)

getUpdatedConnections' :: 
  (M.Map String Integer) ->
  ([DbConnection],[(T.Connection, String)]) -> 
  (Station,Station) -> 
  ([DbConnection],[(T.Connection, String)])
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
     ([DbConnection], [(T.Connection, String)])
getUpdatedConnections stationCache stations = foldl (getUpdatedConnections' stationCache) ([], []) stations
  

connToUpdateForm 
  :: M.Map String Integer -> 
     (T.Connection, String) -> 
     (Integer, Maybe LocalTime, Integer, Integer)
connToUpdateForm stationCache (c, internalName) = (delayTime c, arrivalTime c, connId c, dbId)
  where dbId = stationCache M.! internalName

updateConnections 
  :: Connection -> -- Db connection
     M.Map String Integer -> -- Map between the IP station id and DB station id
     ([DbConnection],[(T.Connection, String)]) -> -- Stations to insert and those to update
     IO ()
updateConnections connection stationCache (added, updated) = do
  executeMany connection connInsertQuery added

  let formattedUpdates = map (connToUpdateForm stationCache) updated
  DT.trace ("updates " ++ (show . length $ formattedUpdates)) $ withTransaction connection (mapM (execute connection connUpdateQuery) formattedUpdates)
  return ()
  --executeMany connection connUpdateQuery formattedUpdates >> return ()
  

persistDelays :: StationCache -> StationCache -> IO ()
persistDelays oldState newState = do
  connection <- connect connectionData
  -- Insert new stations and connections
  stationIdCache <- addStations connection $ getAddedStations oldState newState
  let updatedConns = getUpdatedConnections stationIdCache $ getCommonStations oldState newState
  updateConnections connection stationIdCache updatedConns >> return ()
