module Fetchers where

import Network.HTTP.Conduit
import Language.Haskell.TH.Ppr (bytesToString)
import qualified Data.ByteString.Lazy as BL

import Types

stationUrl = "http://infopasazer.intercity.pl/?p=station&id="
trainUrl = "http://infopasazer.intercity.pl/?p=train&id="

parseResponse :: Response BL.ByteString -> String
parseResponse body = bytesToString . BL.unpack . responseBody $ body

getRequest :: RequestType -> Integer -> IO Request
getRequest reqType stationId
  | reqType == StationRequest = parse stationUrl
  | reqType == TrainRequest   = parse trainUrl 
  where parse url = parseUrl $ url ++ (show stationId)

