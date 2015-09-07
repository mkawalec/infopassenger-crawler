module StationCrawler.IdManipulations (getId, updateIds) where

import StationCrawler.Types
import Types

import Control.Concurrent.STM
import qualified Data.List as L

retryOrQuit :: 
  StationState ->      -- State
  TVar StationState -> -- State tvar, for a cleaner function body
  Int ->               -- Amount of threads waiting
  TVar Int ->          -- A tvar for that
  Int ->               -- Total amount of thrads
  (TVar StationState -> TVar Int -> STM (Maybe (UpdateType, Integer))) -> -- gets an id
  STM (Maybe (UpdateType, Integer)) -- Nothing if all the threads need to be killed
retryOrQuit state stateVar waitCount waitVar workerCount fetchId
  | nothingLeft && waitCount == workerCount = do modifyTVar waitVar (+1)
                                                 return Nothing
  | nothingLeft = retry
  | otherwise   = fetchId stateVar waitVar
    
  where stationsLeft = L.length . stationsLeftToVisit $ state
        trainsLeft   = L.length . trainsLeftToVisit $ state
        nothingLeft  = stationsLeft == 0 && trainsLeft == 0

-- Unqueues the latest id from artifacts left to visit
getStateWithoutId :: UpdateType -> StationState -> StationState
getStateWithoutId updateType state = case updateType of
  TrainUpdate -> state { trainsLeftToVisit = updatedValue }
  StationUpdate -> state { stationsLeftToVisit = updatedValue }
  where updatedValue = tail . accessor $ state
        accessor = getAccessor updateType

-- For a given update type, returns a function for getting ids of remaining artifacts
getAccessor :: UpdateType -> (StationState -> [Integer])
getAccessor TrainUpdate = trainsLeftToVisit
getAccessor StationUpdate = stationsLeftToVisit

-- Get the type of next update. If there are no stations left to visit, get a new
-- station, otherwise visit the train
getUpdateType :: StationState -> UpdateType
getUpdateType state = case L.length . stationsLeftToVisit $ state of
  0 -> TrainUpdate
  otherwise -> StationUpdate

-- Get id of next artifact
retrieveId :: TVar StationState -> TVar Int -> STM (Maybe (UpdateType, Integer))
retrieveId stateVar waitingCount = do
  state <- readTVar stateVar
  let updateType     = getUpdateType state
  let accessor       = getAccessor updateType
  let id             = head . accessor $ state
  let stateWithoutId = getStateWithoutId updateType state
  writeTVar stateVar stateWithoutId
  modifyTVar' waitingCount (subtract 1)
  return $ Just (updateType, id)

-- Returns an id of a next artifact, and checks if we don't have to finish crawling 
-- because we've visited all the stations already
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

