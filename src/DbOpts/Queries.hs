module DbOpts.Queries where

connInsertQuery = "insert into connections (delay, conn_id, train_id, arrival_time, station_id) values (?, ?, ?, ?, ?)"
connUpdateQuery = "UPDATE connections SET delay = ? WHERE arrival_time = ? AND conn_id = ? AND station_id = ?"
connExistenceQuery = "SELECT id FROM connections WHERE conn_id = ? AND train_id = ? AND arrival_time = ? AND station_id = ?"

connSelectQuery = "select c.delay,c.conn_id,c.train_id,c.arrival_time,s.name AS station_name \
                  \ from connections AS c left join stations AS s ON c.station_id=s.id where \
                  \ train_id = ?"
