{-# LANGUAGE RankNTypes, OverloadedStrings #-}

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

    -- * Binary data (PostgreSQL only)
  , byteaPack
  , byteaUnpack

    -- * Reexport
  , module Database.HDBC
  ) where

import Control.Monad
import Control.Monad.IO.Peel
import Control.Monad.Trans
import Data.Word
import Database.HDBC hiding (catchSql, handleSql, throwSqlError)
import Database.HDBC.PostgreSQL
import Text.Printf
import Numeric

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import qualified Database.HDBC as H

import NPaste.Types

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
-- Binary data (postgrsql only)

-- | Convert binary `ByteString` to valid posgresql `SqlValue` (hex escaping)
byteaPack :: B.ByteString -> SqlValue
byteaPack bs = toSql $ "\\x" `B.append` B.concatMap packW8 bs
 where
   packW8 :: Word8 -> B.ByteString
   packW8 = B8.pack . printf "%02x"

-- | Retrieve a binary `ByteString` from a postgresql `SqlValue` (hex escaping)
byteaUnpack :: SqlValue -> B.ByteString
byteaUnpack sql = B.pack . unpackW8 $ fromSql sql
 where
  unpackW8 :: String -> [Word8]
  unpackW8 ('\\':'x':r) = unpackW8 r
  unpackW8 (a:b:c)      = case readHex [a,b] of
                               [(h,"")] -> (h:unpackW8 c)
                               _        -> error $ "invalid sequence: " ++ show [a,b]
  unpackW8 []           = []
  unpackW8 _            = error "uneven number of digits"
