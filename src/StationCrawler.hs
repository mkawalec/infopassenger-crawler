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

import Control.Concurrent (forkIO, killThread, ThreadId)
import Control.Concurrent.STM
import Control.Exception (finally, catch, SomeException)
import Control.Monad.State
import Control.Monad (mapM_)
import System.IO (hFlush, hPutStrLn, stdout)

data StationState = StationState {
  queuedStations :: M.Map StationId Bool
, queuedTrains :: M.Map TrainId Bool
, trainsLeftToVisit :: [TrainId]
, stationsLeftToVisit :: [StationId]
}

data UpdateType = TrainUpdate | StationUpdate

instance Default StationState where 
  def = StationState {
    queuedStations = M.empty
  , queuedTrains = M.empty
  , trainsLeftToVisit = []
  , stationsLeftToVisit = []
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

getUpdateType :: StationState -> UpdateType
getUpdateType state = case L.length . stationsLeftToVisit $ state of
  0 -> TrainUpdate
  otherwise -> StationUpdate

retryOrQuit :: 
  StationState -> 
  TVar StationState ->
  Int -> 
  TVar Int -> 
  Int ->
  (TVar StationState -> TVar Int -> STM (Maybe (UpdateType, Integer))) ->
  STM (Maybe (UpdateType, Integer))
retryOrQuit state stateVar waitCount waitVar workerCount fetchId
  | nothingLeft && waitCount == workerCount = do modifyTVar waitVar (+1)
                                                 return Nothing
  | nothingLeft = retry
  | otherwise = fetchId stateVar waitVar
    
  where stationsLeft = L.length . stationsLeftToVisit $ state
        trainsLeft = L.length . trainsLeftToVisit $ state
        nothingLeft = stationsLeft == 0 && trainsLeft == 0

retrieveId :: TVar StationState -> TVar Int -> STM (Maybe (UpdateType, Integer))
retrieveId stateVar waitingCount = do
  state <- readTVar stateVar
  let updateType = getUpdateType state
  let accessor = getAccessor updateType
  let id = head . accessor $ state
  let stateWithoutId = getStateWithoutId updateType state
  writeTVar stateVar stateWithoutId
  modifyTVar' waitingCount (subtract 1)
  return $ Just (updateType, id)


getId :: TVar StationState -> TVar Int -> Int -> IO (Maybe (UpdateType, Integer))
getId stateVar waitingCount workerCount = do
  atomically $ modifyTVar' waitingCount (+ 1)

  atomically $ do
    state <- readTVar stateVar
    waitCount <- readTVar waitingCount
    retryOrQuit state stateVar waitCount waitingCount workerCount retrieveId
 
updateIds :: TVar StationState -> (StationState -> Integer -> StationState) -> [TrainId] -> STM ()
updateIds stateVar updateFunc ids = do
  state <- readTVar stateVar
  let stateWithIds = foldl updateFunc state ids
  writeTVar stateVar stateWithIds

trainWorker :: Manager -> TVar StationState -> Integer -> IO ()
trainWorker manager stateVar trainId = do
  -- Fetch and parse the page
  trainPage <- getRequest TrainRequest trainId >>= (makeReq manager)
  stationIds <- parseTrain trainPage

  -- Update the ids
  atomically $ updateIds stateVar queueStations stationIds

stationWorker :: Manager -> TVar StationState -> TChan Station -> Integer -> IO ()
stationWorker manager stateVar results stationId = do
  -- Fetch the page
  stationPage <- getRequest StationRequest stationId >>= (makeReq manager)
  -- Parse the page
  parsedStation <- parseStationPage stationId stationPage
  -- Extract train ids from the parsed page
  trainIds <- getTrainIds parsedStation

  -- Update train ids left to fetch
  atomically $ updateIds stateVar queueTrains trainIds
  -- Write the results into results channel
  when (isJust parsedStation) $ atomically $ writeTChan results (fromJust $! parsedStation)

generalWorker :: Manager -> 
  TVar StationState -> 
  TChan Station -> 
  TVar Int ->
  Int ->
  IO ()
generalWorker manager stateVar results waitingCount workerCount = do
  maybeId <- getId stateVar waitingCount workerCount
  case maybeId of
    Just (updateType, id) -> case updateType of
      TrainUpdate -> trainWorker manager stateVar id 
      StationUpdate -> stationWorker manager stateVar results id 
    Nothing -> return ()

  when (isJust maybeId) $ generalWorker manager stateVar results waitingCount workerCount

reportResults :: TChan Station -> TVar [Station] -> IO ()
reportResults c r = forever $ atomically $ do
  result <- readTChan c
  results <- readTVar r
  writeTVar r $ result:results

-- crawl stations starting at a given StationId 
-- (preferably a big station, we don't want closed loops)
crawlStations :: StationId -> IO [Station]
crawlStations stationId = withSocketsDo $ do
  let k = 5
  state <- newTVarIO $ def { stationsLeftToVisit = [stationId] }
  resultsVar <- newTVarIO []
  resultsChannel <- newTChanIO

  workers <- newTVarIO 0
  reporterId <- forkIO $ reportResults resultsChannel resultsVar
  manager <- newManager tlsManagerSettings

  workerIds <- forkTimes k (generalWorker manager state resultsChannel workers k)
  waitFor k workers
  killWorkers $ reporterId:workerIds

  (atomically $ readTVar resultsVar) >>= return

forkTimes :: Int -> IO () -> IO [ThreadId]
forkTimes k act = replicateM k . forkIO $ act
  
waitFor :: Int -> TVar Int -> IO ()
waitFor maxCount alive = atomically $ do
  count <- readTVar alive
  check (count == maxCount + 1)

killWorkers :: [ThreadId] -> IO ()
killWorkers = mapM_ killThread

