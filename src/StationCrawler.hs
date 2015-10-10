module StationCrawler (crawlStations) where

import Types

import StationCrawler.Types
import StationCrawler.Workers (generalWorker)

import Network.HTTP.Conduit (newManager, tlsManagerSettings)
import Network (withSocketsDo)
import qualified Debug.Trace as DT
import Data.Default

import Control.Concurrent (forkIO, killThread, ThreadId)
import Control.Concurrent.STM
import Control.Exception (finally, catch, SomeException)
import Control.Monad.State (forever, replicateM)
import Control.Monad (mapM_)

-- writes results coming from a channel into a TVar
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

  activeWorkers <- newTVarIO 0
  reporterId <- forkIO $ reportResults resultsChannel resultsVar
  manager <- newManager tlsManagerSettings

  workerIds <- forkTimes k (generalWorker manager state resultsChannel activeWorkers k)
  waitFor k activeWorkers
  killWorkers $ reporterId:workerIds

  atomically $ readTVar resultsVar

-- spawn k threads doing act
forkTimes :: Int -> IO () -> IO [ThreadId]
forkTimes k act = replicateM k . forkIO $ act
  
-- wait for the value to exceed max count
waitFor :: Int -> TVar Int -> IO ()
waitFor maxCount alive = atomically $ do
  count <- readTVar alive
  check (count == maxCount + 1)

-- Kills the workers at thread ids specified
killWorkers :: [ThreadId] -> IO ()
killWorkers = mapM_ killThread

