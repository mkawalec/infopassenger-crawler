module StationCrawler.Workers (generalWorker) where

import StationCrawler.Types
import StationCrawler.IdManipulations
import StationCrawler.Queuers (queueStations, queueTrains)
import Types
import Fetchers (getRequest, parseResponse)
import TrainParsers (parseTrain)
import StationParsers (parseStationPage)

import Data.Maybe
import Network.HTTP.Conduit
import Text.XML.HXT.Core hiding (when)
import Text.XML.HXT.TagSoup
import Control.Concurrent.STM
import Control.Monad.State

makeReq manager request = do
  response <- httpLbs request manager
  return $ readString [withParseHTML yes, withWarnings no, withTagSoup] (parseResponse response)

getTrainIds :: Maybe Station -> IO [Integer]
getTrainIds wrappedStation = case wrappedStation of 
  Nothing -> return []
  Just station -> return $ map connId (connections station)

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
