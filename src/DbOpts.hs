module DbOpts(persistDelays) where

import Types hiding (Connection)
import qualified Types as T
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Database.PostgreSQL.Simple
import GHC.Int (Int64)
import Control.Monad

import DbOpts.DbInterface
import DbOpts.UpdatesSolver
import DbOpts.Types
import DbOpts.ConnToDb

connectionData = defaultConnectInfo {
  connectPort     = 2345
, connectUser     = "info"
, connectDatabase = "info"
, connectPassword = "info"
}

persistDelays :: StationCache -> StationCache -> IO ()
persistDelays oldState newState = do
  connection <- connect connectionData

  -- Insert new stations and connections
  stationIdCache <- addStations connection $ getAddedStations oldState newState

  let updatedConns = getUpdatedConnections stationIdCache $ getCommonStations oldState newState
  updateConnections connection stationIdCache updatedConns >> return ()
