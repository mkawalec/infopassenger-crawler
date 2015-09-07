module DbOpts.Types where

import Data.Maybe
import Data.Time.LocalTime (LocalTime)

-- Connection data in a db-ready form
type DbConnection = (Integer, Integer, String, Maybe LocalTime, Integer)
