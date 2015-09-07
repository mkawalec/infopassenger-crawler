module DbOpts.ConnToDb where

import Types
import DbOpts.Types

import Data.Maybe
import Data.Time.LocalTime (LocalTime)
import qualified Data.Map.Strict as M

connToDbConn :: Integer -> Connection -> DbConnection
connToDbConn stationDbId x = (delayTime x, connId x, trainId x, arrivalTime x, stationDbId)

connToUpdateForm 
  :: M.Map String Integer -> 
     (Connection, String) -> 
     (Integer, Maybe LocalTime, Integer, Integer)
connToUpdateForm stationCache (c, internalName) = (delayTime c, arrivalTime c, connId c, dbId)
  where dbId = stationCache M.! internalName
