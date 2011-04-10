{-# LANGUAGE NamedFieldPuns, RankNTypes #-}

module NPaste.Database.Post.Info
  ( -- * Queries
    Query
  , getPostById
  , getPostByMD5
  , getRecentPosts
  , getPostsByUser
  , getGlobalIds
  , getPrivateIds

  , checkCustomId

    -- * Updates
  , Update
  , addPostInfo
  , updatePostInfo
  ) where

import Data.Maybe
import Data.ByteString (ByteString)

import NPaste.Database.Connection
import NPaste.Types
import NPaste.Utils

--------------------------------------------------------------------------------
-- Queries

getPostById :: Maybe User -> String -> Query (Maybe PostInfo)
getPostById mu pid =
  fmap convertListToMaybe $
       querySql "SELECT * FROM HS_PostInfo WHERE p_id = ? AND p_user_id = ?"
                [toSql pid, toSql (maybe (-1) u_id mu)]

getPostByMD5 :: Maybe User -> ByteString -> Query (Maybe PostInfo)
getPostByMD5 mu hash =
  fmap convertListToMaybe $
       querySql "SELECT p_id, p_user_id, p_date, p_type, p_description, p_hidden, p_id_is_global, p_id_is_custom \
                \  FROM posts \
                \ WHERE p_user_id = ? AND p_md5 = ?"
                [toSql (maybe (-1) u_id mu), byteaPack hash]

getRecentPosts :: Maybe User
               -> Int          -- ^ LIMIT
               -> Int          -- ^ OFFSET
               -> Query [PostInfo]
getRecentPosts Nothing limit offset =
  fmap (catMaybes . map convertMaybe)
       (querySql "SELECT * FROM HS_PostInfo \
                 \ORDER BY p_date DESC LIMIT ? OFFSET ?"
                 [toSql limit, toSql offset])
getRecentPosts (Just u) limit offset =
  fmap (catMaybes . map convertMaybe)
       (querySql "SELECT * FROM HS_PostInfo \
                 \WHERE p_user_id = ? \
                 \ORDER BY p_date DESC LIMIT ? OFFSET ?"
                 [toSql (u_id u), toSql limit, toSql offset])

getPostsByUser :: User
               -> Int          -- ^ LIMIT
               -> Int          -- ^ OFFSET
               -> Query [PostInfo]
getPostsByUser User{u_name} limit offset =
  fmap (catMaybes . map convertMaybe)
       (querySql "SELECT * FROM HS_PostInfo \
                 \WHERE p_user_id IN ( SELECT u_id FROM users WHERE u_name = ? ) \
                 \ORDER BY p_date DESC LIMIT ? OFFSET ?"
                 [toSql u_name, toSql limit, toSql offset])

--
-- IDs
--

getGlobalIds :: Query [String]
getGlobalIds =
  fmap (catMaybes . map toStr)
       (querySql "SELECT p_id FROM posts \
                 \WHERE p_id_is_global = 'true'" -- include removed
                 [])
 where
  toStr [sql] = Just $ fromSql sql
  toStr _     = Nothing

getPrivateIds :: User -> Query [String]
getPrivateIds User{ u_id } =
  fmap (catMaybes . map toStr)
       (querySql "SELECT p_id FROM posts \
                 \WHERE p_user_id = ? AND p_id_is_global = 'false'" -- include removed
                 [toSql u_id])
 where
  toStr [sql] = Just $ fromSql sql
  toStr _     = Nothing

checkCustomId :: User -> String -> Query Bool
checkCustomId User{ u_id } pid =
  fmap (null)
       (querySql "SELECT p_id FROM posts \
                 \WHERE p_id = ? AND p_user_id = ?" -- include removed
                 [toSql pid, toSql u_id])


--------------------------------------------------------------------------------
-- Updates

addPostInfo :: ByteString -> PostInfo -> Update ()
addPostInfo md5 p@PostInfo{ p_id, p_user_id, p_date, p_type, p_description, p_hidden, p_id_is_global, p_id_is_custom } = do
  liftIO $ print p
  updateSql_ "INSERT INTO posts (p_id, p_user_id, p_date, p_type, p_description, p_md5, p_hidden, p_id_is_global, p_id_is_custom) \
             \VALUES            (?   , ?        , ?     , ?     , ?            , ?    , ?       , ?             , ?             )"
             [ toSql p_id, toSql p_user_id, toSql p_date, toSql p_type
             , toSql p_description, byteaPack md5, toSql p_hidden, toSql p_id_is_global, toSql p_id_is_custom ]

updatePostInfo :: PostInfo -> Update ()
updatePostInfo PostInfo { p_id, p_user_id, p_type, p_description, p_hidden } =
  updateSql_ "UPDATE posts SET p_type = ?, p_description = ?, p_hidden = ? \
             \WHERE p_id = ? AND p_user_id = ?"
             [ toSql p_type, toSql p_description, toSql p_hidden
             , toSql p_id, toSql p_user_id ]

