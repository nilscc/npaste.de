{-# LANGUAGE RankNTypes #-}

module NPaste.Database.Connection
  ( withConnection
  , updateSql
  , updateSql_
  , querySql
  , Query
  , Update
    -- * SQL Exceptions
  , catchSql
  , handleSql
  , throwSqlError

    -- * Reexport
  , module Database.HDBC
  ) where

import Control.Monad
import Control.Monad.IO.Peel
import Control.Monad.Trans
import Database.HDBC hiding (catchSql, handleSql, throwSqlError)
import Database.HDBC.PostgreSQL

import qualified Database.HDBC as H

type Query  a = (Functor m, MonadPeelIO m) => m a
type Update a = (Functor m, MonadPeelIO m) => m a

conStr :: String
conStr = "host=localhost user=npaste dbname=npaste password=1234"

withConnection :: MonadIO m => (Connection -> IO a) -> m a
withConnection = liftIO . withPostgreSQL conStr

--------------------------------------------------------------------------------
-- SQL

updateSql :: MonadIO m => String -> [SqlValue] -> m Integer
updateSql s v = withConnection $ \c -> do
  i <- run c s v
  commit c
  return i

updateSql_ :: MonadIO m => String -> [SqlValue] -> m ()
updateSql_ s v = updateSql s v >> return ()

querySql :: MonadIO m => String -> [SqlValue] -> m [[SqlValue]]
querySql s v = withConnection $ \c -> quickQuery' c s v


--------------------------------------------------------------------------------
-- Exceptions

catchSql :: MonadPeelIO m => m a -> (SqlError -> m a) -> m a
catchSql m h = do
  k <- peelIO
  join . liftIO $ H.catchSql (k m) (\e -> k (h e))

handleSql :: MonadPeelIO m => (SqlError -> m a) -> m a -> m a
handleSql = flip catchSql

throwSqlError :: MonadIO m => SqlError -> m a
throwSqlError e = do
  liftIO $ H.throwSqlError e
