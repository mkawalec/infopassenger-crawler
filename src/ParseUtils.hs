module ParseUtils where

import qualified Data.List as L
import qualified Network.HTTP.Types.URI as U
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M
import Data.Maybe
import qualified Debug.Trace as DT
import Data.Text.Encoding (encodeUtf8)
import Data.Text (Text)

import Types

extractId :: Text -> TrainId
extractId queryString = read . BC.unpack . fromJust $ param
  where parsed = U.parseQuery . encodeUtf8 $ queryString
        mappedParams = M.fromList parsed
        param = mappedParams M.! "id"
        
