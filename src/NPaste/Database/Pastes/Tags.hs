{-# LANGUAGE NamedFieldPuns #-}

module NPaste.Database.Pastes.Tags
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

addTags :: ID
        -> [String]
        -> Update ()
addTags pid tags =
  forM_ tags $ \t ->
    updateSql_ "INSERT INTO tags (t_Paste_id, t_Paste_user_id, t_tag) \
               \VALUES (?, ?, ?)"
               [ toSql p_id, toSql p_u, toSql t ]
 where
  (p_id, p_u) = case pid of
                     ID                   pid' -> (pid',-1)
                     PrivateID User{u_id} pid' -> (pid',u_id)
