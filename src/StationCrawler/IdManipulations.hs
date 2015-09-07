module StationCrawler.IdManipulations (getId, updateIds) where

import StationCrawler.Types
import Types

import Control.Concurrent.STM
import qualified Data.List as L

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

