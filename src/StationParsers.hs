module StationParsers where

import qualified Data.Text.Lazy as TL
import qualified Data.List as L
import Data.List.Split
import Data.Time.Format (readTime, defaultTimeLocale)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Either as E
import System.IO.Unsafe
import qualified Debug.Trace as DT
import Data.String.Utils as SU

-- HTML parsing
import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs
import Text.HandsomeSoup

import Types
import ParseUtils



genConnection :: [String] -> [String] -> Maybe Connection
genConnection (trainLink:_) (reverse -> (delay:arrivalHour:relation:arrivalDate:_)) = 
    Just $ Connection trainIdVal connDate delayAmount
        where delayAmount = read . head . (splitOn " ") $ delay
              connDate = readTime defaultTimeLocale "%F %H:%M" (arrivalDate ++ " " ++ arrivalHour)
              trainIdVal = extractId [trainLink]
genConnection _ _ = Nothing

getConnections' doc = doc 
    >>> selectNth "table.table-delay" 0
    //> multi (css "tr")
    >>> proc r -> do
        columns <- listA (css "td" //> getText >>. filter isNotEmpty) -< r 
        links <- listA (css "td" //> css "a" >>> getAttrValue "href") -< r
        returnA -< genConnection links columns

getStationData' stationId doc = doc
  >>> selectNth "p.h4" 0 
  //> getText 
  >>. map (\nameString -> Station stationId (extractStationName nameString) [])

extractStationName :: String -> String
extractStationName nameString = SU.strip name
  where name = (splitOn "stacji" nameString) !! 1


mergeConnections :: [Connection] -> [Connection]
mergeConnections trains = M.elems merged
  where resolveConflict = (\first second -> if delayTime first > delayTime second then first else second) 
        mergeSingle = (\cache connection -> 
          M.insertWith resolveConflict (trainId connection) connection cache)
        merged = foldl mergeSingle M.empty trains
  

getConnections :: IOSLA (XIOState ()) XmlTree (NTree XNode) -> IO ([Connection])
getConnections stationPage = do
  connections <- runX . getConnections' $ stationPage
  return . mergeConnections . (map fromJust) . (filter isJust) $ connections


getStationData stationId stationPage = do 
  stations <- runX . getStationData' stationId $ stationPage
  case L.length stations of
    0 -> return Nothing
    otherwise -> return . Just $ stations !! 0
    

parseStationPage stationId stationPage = do
  connections <- getConnections stationPage
  station <- getStationData stationId stationPage
  case station of 
    Nothing -> return Nothing
    Just station -> return . Just $ station { connections = connections }

