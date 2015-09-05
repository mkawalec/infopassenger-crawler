module Main where

import Network (withSocketsDo)

import StationCrawler (crawlStations)
import Types
import qualified Data.List as L
import System.IO (writeFile)
import qualified Data.Map.Strict as M
import DbOpts
import Control.Monad
import Turtle.Prelude (testfile)
import Control.Concurrent (threadDelay)
import MergeStates

createState :: [Station] -> StationCache
createState stations = M.fromList preprocessed
  where filteredStations = filter (\s -> (L.length . stationName $ s) > 0) stations
        preprocessed = map (\x -> (stationName x, x)) filteredStations

queryStations :: StationCache -> IO ()
queryStations previousState = do
  stations <- crawlStations 80416
  putStrLn $ "we have " ++ (show . L.length $ stations) ++ " stations"

  let currentState = mergeStates previousState $ createState stations
  persistDelays previousState currentState
  writeFile "./station-dump" $ show $ map snd (M.toList currentState)

  --threadDelay $ 5 * 60 * 1000000
  queryStations currentState >> return ()
  

main :: IO ()
main = withSocketsDo $ do
  isDump <- testfile "./station-dump"
  oldState <- case isDump of
    True -> readFile "./station-dump" >>= return . createState . read
    False -> return M.empty

  queryStations oldState 
