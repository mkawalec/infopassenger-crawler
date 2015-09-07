module DbOpts.Queries where

import qualified Types as T
import Database.PostgreSQL.Simple

connInsertQuery = "insert into connections (delay, conn_id, train_id, arrival_time, station_id) values (?, ?, ?, ?, ?)"
connUpdateQuery = "UPDATE connections SET delay = ?, arrival_time = ? WHERE conn_id = ? AND station_id = ?"

