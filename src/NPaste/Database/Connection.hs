{-# LANGUAGE RankNTypes, OverloadedStrings #-}

module NPaste.Database.Connection
  ( withConnection

    -- * SQL Updates
  , Update
  , updateSql
  , updateSql_

    -- * SQL Queries
  , Query
  , Select (..)
  , querySql

    -- * SQL Exceptions
  , catchSql
  , handleSql
  , throwSqlError

    -- * Reexport
  , module Database.HDBC
  , module NPaste.Utils.Database
  ) where

import Control.Monad
import Control.Monad.IO.Peel
import Control.Monad.Trans
import Database.HDBC hiding (catchSql, handleSql, throwSqlError)
import Database.HDBC.PostgreSQL

import qualified Database.HDBC as H

import NPaste.Types
import NPaste.Utils.Database

conStr :: String
conStr = "host=localhost user=nils dbname=npaste password=1234"

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


--------------------------------------------------------------------------------
-- SELECT class

-- | Minimal complete definition: `select` and `convertFromSql`. Example
-- implementation:
--
-- > instance Select Foo where
-- >   select         = withSelectStr "SELECT foo FROM foos"
-- >   convertFromSql = convertSqlToFoo
--
-- Example usage:
--
-- > getFooById :: MonadIO m => Id -> m Foo
-- > getFooById id = select "WHERE id = ?" [toSql id]
--
class Select res where

  convertFromSql :: [[SqlValue]] -> res
  select         :: MonadIO m => String -> [SqlValue] -> m res
  fullSelect     :: MonadIO m => String -> [SqlValue] -> m res
  withSelectStr  :: MonadIO m
                 => String        -- ^ "SELECT .. FROM .."
                 -> String        -- ^ "WHERE .." / "JOIN .." etc
                 -> [SqlValue]
                 -> m res

  -- default implementations
  fullSelect    s     v = querySql s v >>= return . convertFromSql
  withSelectStr s1 s2 v = fullSelect (s1++" "++s2) v
