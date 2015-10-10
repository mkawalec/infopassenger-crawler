{-# LANGUAGE RankNTypes #-}
module StationCrawler.Workers (generalWorker) where

import StationCrawler.Types
import StationCrawler.IdManipulations
import StationCrawler.Queuers (queueStations, queueTrains)
import Types
import Fetchers (getRequest)
import TrainParsers (parseTrain)
import StationParsers (parseStationPage)
import Data.Text.Lazy.Encoding (decodeUtf8)

import Data.Maybe
import Network.HTTP.Conduit
import Control.Concurrent.STM
import Control.Monad.State
import qualified Control.Exception as X
import qualified Debug.Trace as DT
import qualified Data.ByteString.Lazy as BL

import Text.XML.Cursor (fromDocument, attribute)
import Text.HTML.DOM (parseLBS)

performReq request manager = httpLbs request manager `X.catch` (handleConnError request manager)


handleConnError :: Request -> Manager -> X.SomeException -> IO (Response BL.ByteString)
handleConnError request manager _ = DT.trace "exception" $ performReq request manager

makeReq manager request = do
  response <- performReq request manager
  return . fromDocument . parseLBS . responseBody $ response

getTrainIds :: Maybe Station -> IO [Integer]
getTrainIds wrappedStation = case wrappedStation of 
  Nothing -> return []
  Just station -> return $ map connId (connections station)

trainWorker :: Manager -> TVar StationState -> Integer -> IO ()
trainWorker manager stateVar trainId = do
  -- Fetch and parse the page
  trainResp <- getRequest TrainRequest trainId >>= makeReq manager
  let stationIds = parseTrain trainResp

  -- Update the ids
  atomically $ updateIds stateVar queueStations stationIds

stationWorker :: Manager -> TVar StationState -> TChan Station -> Integer -> IO ()
stationWorker manager stateVar results stationId = do
  -- Fetch the page
  stationResp <- getRequest StationRequest stationId >>= makeReq manager
  -- Parse the page
  let parsedStation = parseStationPage stationId stationResp
  -- Extract train ids from the parsed page
  trainIds <- getTrainIds parsedStation

  -- Update train ids left to fetch
  atomically $ updateIds stateVar queueTrains trainIds
  -- Write the results into results channel
  when (isJust parsedStation) $ atomically $ writeTChan results (fromJust $! parsedStation)

-- A worker that can either crawl a station or a train, depending on need
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
