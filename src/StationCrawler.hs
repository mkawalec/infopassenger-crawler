module StationCrawler where

import Types
import Fetchers (getRequest, parseResponse)
import StationParsers (parseStationPage)
import TrainParsers (parseTrain)

import Network.HTTP.Conduit
import Network (withSocketsDo)
import Text.XML.HXT.Core hiding (when)
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.Default
import Control.Monad (foldM)
import Data.List
import Data.Maybe
import qualified Debug.Trace as DT

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad.State
import System.IO (hFlush, hPutStrLn, stdout)

data StationState = StationState {
  queuedStations :: M.Map StationId Bool
, queuedTrains :: M.Map TrainId Bool
, trainsLeftToVisit :: [TrainId]
, stationsLeftToVisit :: [StationId]
, results :: [Maybe Station]
}

data UpdateType = TrainUpdate | StationUpdate

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


getTrainIds :: Maybe Station -> IO [Integer]
getTrainIds wrappedStation = case wrappedStation of 
  Nothing -> return []
  Just station -> return $ map trainId (connections station)

getStateWithoutId :: UpdateType -> StationState -> StationState
getStateWithoutId updateType state = case updateType of
  TrainUpdate -> state { trainsLeftToVisit = updatedValue }
  StationUpdate -> state { stationsLeftToVisit = updatedValue }
  where updatedValue = tail . accessor $ state
        accessor = getAccessor updateType

getAccessor :: UpdateType -> (StationState -> [Integer])
getAccessor TrainUpdate = trainsLeftToVisit
getAccessor StationUpdate = stationsLeftToVisit

getId :: UpdateType -> TVar StationState -> STM Integer
getId updateType stateVar = do
  let accessor = getAccessor updateType
  state <- readTVar stateVar
  when ((L.length . accessor $ state) == 0) $
    retry

  let id = head . accessor $ state
  let stateWithoutId = getStateWithoutId updateType state
  writeTVar stateVar stateWithoutId
  return id
 
updateIds :: TVar StationState -> (StationState -> Integer -> StationState) -> [TrainId] -> STM ()
updateIds stateVar updateFunc ids = do
  state <- readTVar stateVar
  let stateWithIds = foldl updateFunc state ids
  writeTVar stateVar stateWithIds

trainWorker :: Manager -> TVar StationState -> IO ()
trainWorker manager stateVar = do
  -- Get the train id
  trainId <- atomically $ getId TrainUpdate stateVar
  -- Fetch and parse the page
  trainPage <- getRequest TrainRequest trainId >>= (makeReq manager)
  stationIds <- parseTrain trainPage

  -- Update the ids
  atomically $ updateIds stateVar queueStations stationIds
  trainWorker manager stateVar

stationWorker :: Manager -> TVar StationState -> TChan Station -> IO ()
stationWorker manager stateVar results = do
  -- Get the station Id
  stationId <- atomically $ getId StationUpdate stateVar
  -- Fetch the page
  stationPage <- getRequest StationRequest stationId >>= (makeReq manager)
  -- Parse the page
  parsedStation <- parseStationPage stationId stationPage
  -- Extract train ids from the parsed page
  trainIds <- getTrainIds parsedStation

  -- Update train ids left to fetch
  atomically $ updateIds stateVar queueTrains trainIds
  -- Write the results into results channel
  when (isJust parsedStation) $ atomically $ writeTChan results (fromJust parsedStation)
  stationWorker manager stateVar results

reportResults :: TChan Station -> IO ()
reportResults c = forever $
  atomically (readTChan c) >>= putStrLn . show >> hFlush stdout

-- crawl stations starting at a given StationId (preferably a big station)
crawlStations :: StationId -> IO [Station]
crawlStations stationId = withSocketsDo $ do
  let k = 2
  state <- newTVarIO $ def { stationsLeftToVisit = [stationId] }
  manager <- newManager tlsManagerSettings
  results <- newTChanIO

  stationWorkers <- newTVarIO k
  trainWorkers <- newTVarIO k
  forkIO $ reportResults results

  forkTimes k stationWorkers (stationWorker manager state results)
  forkTimes k trainWorkers (trainWorker manager state)
  waitFor stationWorkers

  return []

modifyTVar_ :: TVar a -> (a -> a) -> STM ()
modifyTVar_ tv f = readTVar tv >>= writeTVar tv . f

forkTimes :: Int -> TVar Int -> IO () -> IO ()
forkTimes k alive act = replicateM_ k . forkIO $
  act `finally` (atomically $ modifyTVar_ alive (subtract 1))
  
waitFor :: TVar Int -> IO ()
waitFor alive = atomically $ do
  count <- readTVar alive
  check (count == 0)

  

