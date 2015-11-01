module Main where

import Network (withSocketsDo)

import qualified Data.List as L
import System.IO (writeFile)
import qualified Data.Map.Strict as M
import qualified Debug.Trace as DT

import StationCrawler (crawlStations)
import Types
import DbOpts
import API (apiServer)

import Control.Monad
import Turtle.Prelude (testfile)
import Control.Concurrent (threadDelay)
import MergeStates
import Data.Time.Clock

import Control.Concurrent (forkIO)


createState :: [Station] -> StationCache
createState stations = M.fromList preprocessed
  where filteredStations = filter (\s -> (L.length . stationName $ s) > 0) stations
        preprocessed = map (\x -> (stationName x, x)) filteredStations

-- |Measures the time it takes to execute a given action,
-- then reports the results to console in a nice state
time :: String -> IO a -> IO a
time actionName a = do
  startTime <- getCurrentTime
  v <- a
  endTime <- getCurrentTime

  putStrLn $ actionName ++ " took " ++ (show $ diffUTCTime endTime startTime)
  return v


queryStations :: StationCache -> IO ()
queryStations previousState = do
  stations <- time "crawl" $ crawlStations 80416

  let currentState = mergeStates previousState $ createState stations
  putStrLn $ "we haz " ++ (show . L.length . M.keys $ currentState) ++ " stations in state"

  time "db update" $ persistDelays previousState currentState
  writeFile "./station-dump" $ show $ map snd (M.toList currentState)

  threadDelay $ 3 * 60 * 1000000
  putStrLn "out from sleep"
  void $ queryStations currentState

main :: IO ()
main = withSocketsDo $ do
  isDump <- testfile "./station-dump"
  oldState <- case isDump of
    True -> readFile "./station-dump" >>= return . createState . read
    False -> return M.empty

  forkIO apiServer
  queryStations oldState 
