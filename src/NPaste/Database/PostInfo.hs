module NPaste.Database.PostInfo where

import Control.Monad.Trans
import Data.Maybe

import NPaste.Database.Connection
import NPaste.Types
import NPaste.Utils

postById :: (Functor m, MonadIO m) => String -> m (Maybe PostInfo)
postById pid = do
  fmap (convertListToMaybe)
       (querySql "SELECT p_id, p_user_id, p_date, p_type, p_description, p_hidden \
                 \FROM posts WHERE p_id = ?"
                 [toSql pid])

recentPosts :: (Functor m, MonadIO m)
            => Int          -- ^ LIMIT
            -> Int          -- ^ OFFSET
            -> m [PostInfo]
recentPosts limit offset = do
  fmap (catMaybes . map convertMaybe)
       (querySql "SELECT p_id, p_user_id, p_date, p_type, p_description, p_hidden \
                 \FROM posts ORDER BY p_date DESC LIMIT ? OFFSET ?"
                 [toSql limit, toSql offset])

postsByUser :: (Functor m, MonadIO m)
            => String       -- ^ user name
            -> Int          -- ^ LIMIT
            -> Int          -- ^ OFFSET
            -> m [PostInfo]
postsByUser uname limit offset = do
  fmap (catMaybes . map convertMaybe)
       (querySql "SELECT p_id, p_user_id, p_date, p_type, p_description, p_hidden \
                 \FROM posts WHERE p_user_id IN ( SELECT u_id FROM users WHERE u_name = ? ) \
                 \ORDER BY p_date DESC LIMIT ? OFFSET ?"
                 [toSql uname, toSql limit, toSql offset])
