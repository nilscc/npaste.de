{-# LANGUAGE NamedFieldPuns #-}

module NPaste.Database.Posts.Tags
  ( -- * Queries
    -- * Updates
    addTags
  ) where

import Control.Monad
-- import Data.Maybe

import NPaste.Database.Connection
import NPaste.Types
-- import NPaste.Utils


--------------------------------------------------------------------------------
-- Queries



--------------------------------------------------------------------------------
-- Updates

addTags :: Maybe User
        -> String
        -> [String]
        -> Update ()
addTags mu pid tags =
  forM_ tags $ \t ->
    updateSql_ "INSERT INTO tags (t_post_id, t_post_user_id, t_tag) \
               \VALUES (?, ?, ?)"
               [ toSql pid, toSql (maybe (-1) u_id mu), toSql t ]
