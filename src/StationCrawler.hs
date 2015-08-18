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
import Control.Exception (finally, catch, SomeException)
import Control.Monad.State
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

getId :: TVar StationState -> TVar Int -> IO (UpdateType, Integer)
getId stateVar waitingCount = do
  atomically $ modifyTVar_ waitingCount (+ 1)

  atomically $ do
    state <- readTVar stateVar
    when ((L.length . stationsLeftToVisit $ state) == 0 && 
        ((L.length . trainsLeftToVisit $ state) == 0)) $
      retry

    let updateType = getUpdateType state
    let accessor = getAccessor updateType
    let id = head . accessor $ state
    let stateWithoutId = getStateWithoutId updateType state
    writeTVar stateVar stateWithoutId
    modifyTVar_ waitingCount (subtract 1)
    return (updateType, id)
 
updateIds :: TVar StationState -> (StationState -> Integer -> StationState) -> [TrainId] -> STM ()
updateIds stateVar updateFunc ids = do
  state <- readTVar stateVar
  let stateWithIds = foldl updateFunc state ids
  writeTVar stateVar stateWithIds

trainWorker :: Manager -> TVar StationState -> Integer -> IO ()
trainWorker manager stateVar trainId = do
  putStrLn "train worker" >> hFlush stdout
  -- Fetch and parse the page
  trainPage <- getRequest TrainRequest trainId >>= (makeReq manager)
  stationIds <- parseTrain trainPage

  -- Update the ids
  atomically $ updateIds stateVar queueStations stationIds

stationWorker :: Manager -> TVar StationState -> TChan Station -> Integer -> IO ()
stationWorker manager stateVar results stationId = do
  putStrLn "worker" >> hFlush stdout
  -- Fetch the page
  stationPage <- getRequest StationRequest stationId >>= (makeReq manager)
  putStrLn "worker2" >> hFlush stdout
  -- Parse the page
  putStrLn "worker3" >> hFlush stdout
  parsedStation <- parseStationPage stationId stationPage
  putStrLn "worker4" >> hFlush stdout
  -- Extract train ids from the parsed page
  trainIds <- getTrainIds parsedStation
  putStrLn "worker5" >> hFlush stdout

  -- Update train ids left to fetch
  putStrLn "getstation" >> hFlush stdout
  atomically $ updateIds stateVar queueTrains trainIds
  -- Write the results into results channel
  when (isJust parsedStation) $ atomically $ writeTChan results (fromJust $! parsedStation)

generalWorker :: Manager -> TVar StationState -> TChan Station -> TVar Int -> IO ()
generalWorker manager stateVar results waitingCount = do
  putStrLn "before getId" >> hFlush stdout
  (updateType, id) <- getId stateVar waitingCount
  putStrLn ("inloop " ++ show id) >> hFlush stdout
  case updateType of
    TrainUpdate -> trainWorker manager stateVar id
    StationUpdate -> stationWorker manager stateVar results id

  putStrLn "spawning new" >> hFlush stdout
  generalWorker manager stateVar results waitingCount

reportResults :: TChan Station -> IO ()
reportResults c = forever $
  atomically (readTChan c) >>= putStrLn . show >> hFlush stdout

-- crawl stations starting at a given StationId 
-- (preferably a big station, we don't want closed loops)
crawlStations :: StationId -> IO [Station]
crawlStations stationId = withSocketsDo $ do
  let k = 50
  state <- newTVarIO $ def { stationsLeftToVisit = [stationId] }
  manager <- newManager tlsManagerSettings
  results <- newTChanIO

  workers <- newTVarIO 0
  --forkIO $ reportResults results

  forkTimes k (generalWorker manager state results workers)
  waitFor k workers

  newVar <- atomically $ readTVar workers
  DT.trace ("died " ++ show newVar) $ hFlush stdout 
  return []

modifyTVar_ :: TVar a -> (a -> a) -> STM ()
modifyTVar_ tv f = readTVar tv >>= writeTVar tv . f

forkTimes :: Int -> IO () -> IO ()
forkTimes k act = replicateM_ k . forkIO $ act
  
waitFor :: Int -> TVar Int -> IO ()
waitFor maxCount alive = atomically $ do
  count <- readTVar alive
  check (count == maxCount + 1)

  

