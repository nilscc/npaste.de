{-# LANGUAGE NamedFieldPuns #-}

module NPaste.Database.Post.Replies
  ( -- * Queries
    getReplies
    -- * Updates
  , addReplies
  ) where

import Control.Monad
import Data.Maybe

import NPaste.Database.Connection
import NPaste.Types
import NPaste.Utils


--------------------------------------------------------------------------------
-- Queries

getReplies :: PostInfo -> Query [PostInfo]
getReplies PostInfo{ p_id, p_user_id } =
  fmap (catMaybes . map convertMaybe) $
       querySql "SELECT * FROM HS_PostInfo \
                \WHERE (p_id, p_user_id) IN ( SELECT r_reply_post_id, r_reply_post_user_id \
                \                               FROM replies \
                \                              WHERE r_post_id = ? AND r_post_user_id = ? \
                \                           )"
                [ toSql p_id, toSql p_user_id ]


--------------------------------------------------------------------------------
-- Updates

addReplies :: Maybe User
           -> String
           -> [(Maybe User, String)]
           -> Update ()
addReplies mu pid rpls =
  forM_ rpls $ \(mru, mpid) ->
    updateSql_ "INSERT INTO replies (r_post_id, r_post_user_id, r_reply_post_id, r_reply_post_user_id) \
               \VALUES (?, ?, ?, ?)"
               [ toSql pid,  toSql (maybe (-1) u_id mu)
               , toSql mpid, toSql (maybe (-1) u_id mru) ]
