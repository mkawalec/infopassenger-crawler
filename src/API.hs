module API (apiServer) where

import API.Types
import Web.Scotty
import Data.Aeson (encode)
import Database.PostgreSQL.Simple

import Data.Text.Lazy (unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Control.Monad.IO.Class

import DbOpts (getDBConnection)
import DbOpts.Queries (connSelectQuery)


getConnections :: String -> IO [APIConnection]
getConnections trainid = do
  dbConn <- getDBConnection
  query dbConn connSelectQuery (Only trainid)


apiServer :: IO ()
apiServer = scotty 12345 $ do
  get "/" $ do
    trainid <- param "trainid"
    connections <- liftIO (getConnections $ unpack trainid)
    html . decodeUtf8 . encode $ connections

