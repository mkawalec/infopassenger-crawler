module TrainParsers where

import Text.XML.HXT.Core
import Text.HandsomeSoup
import qualified Data.ByteString as B

import ParseUtils (extractId)

parseTrain' doc = doc
  >>> css "table.table-delay"
  //> multi (css "tr")
  >>> proc r -> do
      id <- (css "a" >>> getAttrValue "href" >. extractId) -< r
      returnA -< id

parseTrain trainPage = runX . parseTrain' $ trainPage
