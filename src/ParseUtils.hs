module ParseUtils where

import qualified Data.List as L
import qualified Network.HTTP.Types.URI as U
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M
import Text.XML.HXT.Core
import Text.HandsomeSoup
import Data.Maybe
import qualified Debug.Trace as DT

import Types

extractId :: [String] -> TrainId
extractId (queryString:_) = read . BC.unpack . fromJust $ param
  where parsed = U.parseQuery . BC.pack $ queryString
        mappedParams = M.fromList parsed
        param = mappedParams M.! "id"
        

isNotEmpty :: String -> Bool
isNotEmpty col = if length common == 0 then True else False
    where common = L.intersect col "\r\n"

selectNth selector idx = (css selector) >>. (\x -> if idx >= L.length x then [] else DT.trace (show . L.length $ x) $ [x !! idx])
