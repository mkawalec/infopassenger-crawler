module TrainParsers where

import qualified Data.ByteString as B
import qualified Debug.Trace as DT

import Text.XML.Cursor (fromDocument, attribute, Cursor)
import Text.HTML.DOM (parseLBS)
import Text.XML.Scraping
import Text.XML.Selector.TH
import Text.XML (Element, elementAttributes)
import Data.Maybe

import ParseUtils (extractId)
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as DL
import Data.Text (Text)

parseTrain :: Cursor -> [Integer]
parseTrain rootNode = map extractId linkContents
  where matchingNodes = queryT [jq| table.table-delay tr |] rootNode
        links = concat $ map (queryT [jq| a |]) matchingNodes
        linkContents = concat $ map (attribute "href") links
