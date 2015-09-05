module Fetchers where

import Network.HTTP.Conduit
import Language.Haskell.TH.Ppr (bytesToString)
import qualified Data.ByteString.Lazy as BL
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Text.Lazy (unpack)

import Types

stationUrl = "http://infopasazer.intercity.pl/?p=station&id="
trainUrl = "http://infopasazer.intercity.pl/?p=train&id="

parseResponse :: Response BL.ByteString -> String
parseResponse body = unpack . decodeUtf8 . responseBody $ body

getRequest :: RequestType -> Integer -> IO Request
getRequest reqType stationId
  | reqType == StationRequest = parse stationUrl
  | reqType == TrainRequest   = parse trainUrl 
  where parse url = parseUrl $ url ++ (show stationId)

