module NPaste.Database.Connection
  ( withConnection
  , updateSql
  , querySql
  , module Database.HDBC
  ) where

import Database.HDBC
import Database.HDBC.PostgreSQL
import Control.Monad.Trans

conStr :: String
conStr = "host=localhost user=npaste dbname=npaste password=1234"

withConnection :: MonadIO m => (Connection -> IO a) -> m a
withConnection = liftIO . withPostgreSQL conStr

updateSql :: MonadIO m => String -> [SqlValue] -> m Integer
updateSql s v = withConnection $ \c -> run c s v

querySql :: MonadIO m => String -> [SqlValue] -> m [[SqlValue]]
querySql s v = withConnection $ \c -> quickQuery' c s v
