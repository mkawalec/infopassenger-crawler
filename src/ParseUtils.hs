module ParseUtils where

import qualified Network.HTTP.Types.URI as U
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M
import qualified Debug.Trace as DT
import Data.Maybe
import Data.Text.Encoding (encodeUtf8)
import Data.Text (Text)

import Types

extractId :: Text -> TrainId
extractId queryString = if length idValue > 0 then read idValue else -1
  where parsed = U.parseQuery . encodeUtf8 $ queryString
        mappedParams = M.fromList parsed
        idValue = case "id" `M.lookup` mappedParams of
          Nothing -> "-1"
          Just n  -> BC.unpack . fromJust $ n

        
