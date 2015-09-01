module Main where

import Network (withSocketsDo)

import StationCrawler (crawlStations)
import Types
import MergeDelays
import qualified Data.List as L
import System.IO (writeFile)
import qualified Data.Map.Strict as M

main :: IO ()
main = withSocketsDo $ do
    stations <- crawlStations 80416
    putStrLn $ "we have " ++ (show . L.length $ stations) ++ " stations"
    writeFile "./dump" (show stations)
    {-oldStationsS <- readFile "./dump0"
    newStationsS <- readFile "./dump1"

    let oldStations = (read oldStationsS) :: [Station]
    let newStations = (read newStationsS) :: [Station]

    let initDelays = computeCurrentDelays M.empty oldStations
    let finalDelays = computeCurrentDelays initDelays newStations

    putStrLn $ show $ (map (\x -> snd x) $ M.toList finalDelays)-}


