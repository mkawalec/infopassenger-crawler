module DbOpts.DbInterface (updateConnections, addStations) where

import DbOpts.Types
import DbOpts.Queries
import DbOpts.ConnToDb
import Types hiding (Connection)
import qualified Types as T

import Database.PostgreSQL.Simple
import qualified Data.Map.Strict as M
import qualified Debug.Trace as DT

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

getConnections :: [Station] -> [DbStation] -> ([DbConnection], M.Map String Integer)
getConnections stations dbStations = (foldl (mapConnections nameCache) [] stations, nameCache)
  where namePairs = map (\x -> (dbStationName x, dbStationDbId x)) dbStations
        nameCache = M.fromList namePairs

mapConnections :: M.Map String Integer -> [DbConnection] -> Station -> [DbConnection]
mapConnections stationCache acc station = dbConns ++ acc
  where conns = connections station
        stationDbId = stationCache M.! (stationName station)
        dbConns = map (connToDbConn stationDbId) conns

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
  
getStations :: Connection -> IO [T.DbStation]
getStations c = query_ c "SELECT id, name, station_id FROM stations"
