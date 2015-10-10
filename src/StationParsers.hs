module StationParsers where

import qualified Data.Text.Lazy as TL
import qualified Data.List as L
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Either as E
import System.IO.Unsafe
import qualified Debug.Trace as DT
import qualified Data.ByteString.Lazy as BL

import Types
import ParseUtils

import Text.XML.Cursor (Cursor, attribute, hasAttribute)
import Text.XML.Selector.TH
import Text.XML.Scraping (innerText)
import Data.Text (Text, splitOn, unpack, strip)

genConnection :: ([Text], [Text]) -> Maybe Connection
genConnection ((trainLink:_), cols) = 
    Just $ Connection connId trainId connDate delayAmount
        where (delay:arrivalHour:relation:arrivalDate:_) = reverse cols
              delayAmount = read . unpack . head . (splitOn " ") $ delay
              timeParser = parseTimeM True defaultTimeLocale "%F %H:%M"
              connDate =  timeParser (unpack arrivalDate ++ " " ++ unpack arrivalHour)
              connId = extractId trainLink
              trainId = unpack . strip . head $ cols
genConnection _ = Nothing

extractStationName :: Text -> String
extractStationName nameString = unpack $ strip name
  where name = (splitOn "stacji" nameString) !! 1


mergeConnections :: [Connection] -> [Connection]
mergeConnections trains = M.elems merged
  where resolveConflict = (\first second -> if delayTime first > delayTime second then first else second) 
        mergeSingle = (\cache connection -> 
          M.insertWith resolveConflict (trainId connection) connection cache)
        merged = foldl mergeSingle M.empty trains
  
getConnections :: Cursor -> [Connection]
getConnections rootNode = mergeConnections . (map fromJust) . (filter isJust) $ connections
  where arrivalsTable = head $ queryT [jq| table.table-delay |] rootNode
        arrivals = map (queryT [jq| td |]) $ queryT [jq| tr |] arrivalsTable
        columns = map (map (strip . TL.toStrict . innerText)) arrivals
        extractHrefs = (\row -> concat $ map (attribute "href") $ queryT [jq| a |] row)
        links = map (concat . (map extractHrefs)) arrivals
        connections = map genConnection (zip links columns)

getStationData :: StationId -> Cursor -> Maybe Station
getStationData stationId rootNode = case L.length stations of
  0 -> Nothing
  otherwise -> Just . head $ stations

  where header = queryT [jq| p.h4 |] rootNode
        extractName = extractStationName . TL.toStrict . innerText
        stations = map (\name -> Station stationId (extractName name) []) header

parseStationPage :: StationId -> Cursor -> Maybe Station
parseStationPage stationId stationPage = do
  let connections = getConnections stationPage
  station <- getStationData stationId stationPage 
  return $ station { connections = connections }

