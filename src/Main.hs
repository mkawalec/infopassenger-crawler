module Main where

import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Lazy as TL
import Language.Haskell.TH.Ppr (bytesToString)
import Network (withSocketsDo)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Time.LocalTime (LocalTime)
import qualified Data.List as L
import Data.List.Split
import Data.Time.Format (readTime, defaultTimeLocale)
import qualified Debug.Trace as TR

-- HTML parsing
import Text.XML.HXT.Core
import Text.HandsomeSoup

stationUrl = "http://infopasazer.intercity.pl/?p=station&id="

parseResponse :: Response BL.ByteString -> String
parseResponse body = bytesToString . BL.unpack . responseBody $ body

getStationRequest :: Integer -> IO Request
getStationRequest stationId = parseUrl $ stationUrl ++ (show stationId)

isNotEmpty :: String -> Bool
isNotEmpty col = if length common == 0 then True else False
    where common = L.intersect col "\r\n"

genConnection :: [String] -> [String] -> Maybe Connection
genConnection (trainLink:_) (reverse -> (delay:arrivalHour:relation:arrivalDate:_)) = 
    Just $ Connection trainIdVal connDate delayAmount
        where delayAmount = read . head . (splitOn " ") $ delay
              connDate = readTime defaultTimeLocale "%F %H:%M" (arrivalDate ++ " " ++ arrivalHour)
              trainIdVal = read $ (splitOn "=" trainLink) L.!! 2
genConnection _ _ = Nothing


getConnections doc = doc 
    >>> css "table.table-delay" 
    //> multi (css "tr")
    >>> proc r -> do
        columns <- listA (css "td" //> getText >>. filter isNotEmpty) -< r 
        links <- listA (css "td" //> css "a" >>> getAttrValue "href") -< r
        returnA -< genConnection links columns

data Connection = Connection {
    trainId :: Integer,
    arrivalTime :: LocalTime,
    delayTime :: Integer
} deriving (Show, Ord, Eq)



main :: IO ()
main = withSocketsDo $ do
    request <- getStationRequest 80416
    manager <- newManager tlsManagerSettings

    response <- httpLbs request manager
    let doc = readString [withParseHTML yes, withWarnings no] (parseResponse response)
    let conns = getConnections doc
    formatted <-  runX . getConnections $ doc
    mapM_ (putStrLn . show) formatted
