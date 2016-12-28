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
  --, throwSqlError
  , runWithEither
  , runWithId
  , runWithMaybe


    -- * Reexport
  , module Database.HDBC
  ) where

--import Control.Monad
import Control.Monad.Trans
import Database.HDBC hiding (catchSql, handleSql, throwSqlError)
import Database.HDBC.PostgreSQL

import qualified Database.HDBC as H

import NPaste.Types

conStr :: String
conStr = "host=localhost user=nils dbname=npaste-test password=1234"

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

catchSql :: IO a -> (SqlError -> IO b) -> ExceptT b IO a
catchSql m h = do
  result <- liftIO $ H.catchSql (Right `fmap` m) (fmap Left . h)
  case result of
    Right ok -> return ok
    Left err -> throwError err

handleSql :: (SqlError -> IO b) -> IO a -> ExceptT b IO a
handleSql = flip catchSql

--throwSqlError :: SqlError -> ExceptT b IO a
--throwSqlError e = H.throwSqlError e

runWithEither :: ExceptT b m a -> m (Either b a)
runWithEither = runExceptT

runWithId :: Functor m => ExceptT a m a -> m a
runWithId = fmap (either id id) . runExceptT

runWithMaybe :: Functor m => ExceptT b m a -> m (Maybe a)
runWithMaybe = fmap (either (const Nothing) Just) . runExceptT

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
