module Main where

import Network (withSocketsDo)

import StationCrawler (crawlStations)
import Types
import qualified Data.List as L


main :: IO ()
main = withSocketsDo $ do
    formatted <- crawlStations 80416
    let names = map stationName formatted
    mapM_ (putStrLn ) names

    putStrLn (show . L.length $ formatted)
