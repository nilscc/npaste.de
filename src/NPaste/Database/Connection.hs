{-# LANGUAGE RankNTypes, OverloadedStrings #-}

module NPaste.Database.Connection
  ( -- * SQL Updates
    Update
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

updateSql :: String -> [SqlValue] -> Update Integer
updateSql s v = Update $ withConnection $ \c -> do
  i <- run c s v
  commit c
  return i

updateSql_ :: String -> [SqlValue] -> Update ()
updateSql_ s v = updateSql s v >> return ()

querySql :: String -> [SqlValue] -> Query [[SqlValue]]
querySql s v = Query $ withConnection $ \c -> quickQuery' c s v


--------------------------------------------------------------------------------
-- Exceptions

class CatchSql m where
  catchSql :: m a -> (SqlError -> IO b) -> ExceptT b m a

instance CatchSql Query where
  catchSql (Query m) h = do
    result <- liftIO $ (Right `fmap` m) `H.catchSql` (fmap Left . h)
    case result of
      Right ok -> return ok
      Left err -> throwError err

instance CatchSql Update where
  catchSql (Update m) h = do
    result <- liftIO $ (Right `fmap` m) `H.catchSql` (fmap Left . h)
    case result of
      Right ok -> return ok
      Left err -> throwError err

handleSql :: CatchSql t => (SqlError -> IO b) -> t a -> ExceptT b t a
handleSql = flip catchSql

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
  select         :: String -> [SqlValue] -> Query res
  fullSelect     :: String -> [SqlValue] -> Query res
  withSelectStr  :: String        -- ^ "SELECT .. FROM .."
                 -> String        -- ^ "WHERE .." / "JOIN .." etc
                 -> [SqlValue]
                 -> Query res

  -- default implementations
  fullSelect    s     v = querySql s v >>= return . convertFromSql
  withSelectStr s1 s2 v = fullSelect (s1++" "++s2) v
