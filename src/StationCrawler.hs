module StationCrawler where

import Types
import Fetchers (getRequest, parseResponse)
import StationParsers (parseStationPage)
import TrainParsers (parseTrain)

import Network.HTTP.Conduit
import Network (withSocketsDo)
import Text.XML.HXT.Core
import qualified Data.Map.Strict as M
import Data.Default
import Control.Monad (foldM)
import Data.List
import Data.Maybe
import qualified Debug.Trace as DT

data StationState = StationState {
  queuedStations :: M.Map StationId Bool
, queuedTrains :: M.Map TrainId Bool
, trainsLeftToVisit :: [TrainId]
, stationsLeftToVisit :: [StationId]
, results :: [Maybe Station]
}

instance Default StationState where 
  def = StationState {
    queuedStations = M.empty
  , queuedTrains = M.empty
  , trainsLeftToVisit = []
  , stationsLeftToVisit = []
  , results = []
  }

makeReq manager request = do
  response <- httpLbs request manager
  return $ readString [withParseHTML yes, withWarnings no] (parseResponse response)
  
queueTrains :: StationState -> TrainId -> StationState
queueTrains state trainId = if not . M.member trainId $ visited
  then
    state { 
      trainsLeftToVisit = trainId : (trainsLeftToVisit state) 
    , queuedTrains = M.insert trainId True visited
    }
  else
    state    
  where visited = queuedTrains state

queueStations :: StationState -> StationId -> StationState
queueStations state stationId = if not . M.member stationId $ visited
  then
    state { 
      stationsLeftToVisit = stationId : (stationsLeftToVisit state)
    , queuedStations = M.insert stationId True visited
    }
  else
    state
  where visited = queuedStations state

visitTrain :: Manager -> StationState -> TrainId -> IO StationState
visitTrain manager state trainId = do
  trainPage <- getRequest TrainRequest trainId >>= (makeReq manager)
  stationIds <- parseTrain trainPage

  return $ foldl queueStations state stationIds

getTrainIds :: Maybe Station -> IO [Integer]
getTrainIds wrappedStation = case wrappedStation of 
  Nothing -> return []
  Just station -> return $ map trainId (connections station)

visitStation :: StationState -> Manager -> IO StationState
visitStation state manager = do
  let stationId = head . stationsLeftToVisit $ state
  stationPage <- getRequest StationRequest stationId >>= (makeReq manager)
  parsedStation <- parseStationPage stationId stationPage
  trainIds <- getTrainIds parsedStation

  let stateWithStation = state { 
    results = parsedStation : (results state)
  , stationsLeftToVisit = tail . stationsLeftToVisit $ state
  }
  -- Newly discovered trains are added to a list
  let stateWithTrains = DT.trace ("stations left: " ++ (show . length . stationsLeftToVisit $ stateWithStation)) $ foldl queueTrains stateWithStation trainIds

  -- Fetch all trains we haven't fetched yet
  stateWithVisitedTrains <- foldM (visitTrain manager) stateWithTrains (trainsLeftToVisit stateWithTrains)
  let stateWithoutQueuedTrains = stateWithVisitedTrains { trainsLeftToVisit = [] }

  if (length . stationsLeftToVisit $ stateWithoutQueuedTrains) > 0
  then
    visitStation stateWithoutQueuedTrains manager
  else
    return stateWithoutQueuedTrains

-- crawl stations starting at a given StationId (preferably a big station)
crawlStations :: StationId -> IO [Station]
crawlStations stationId = withSocketsDo $ do
  manager <- newManager tlsManagerSettings
  let state = def { stationsLeftToVisit = [stationId] }

  resultState <- visitStation state manager
  return . catMaybes . results $ resultState

  

  

