{-# LANGUAGE NamedFieldPuns #-}

module NPaste.Database.Pastes.Tags
  ( -- * Queries
    getTags
    -- * Updates
  , addTags
  ) where

import Control.Monad
import Data.Maybe

import NPaste.Database.Connection
import NPaste.Types


--------------------------------------------------------------------------------
-- Queries

getTags :: Id -> Query [String]
getTags pid =
  fmap (catMaybes . map convert) $
    querySql "SELECT tag FROM tags WHERE id = ?" [ toSql pid ]
 where
  convert [SqlString s] = Just s
  convert _             = Nothing


--------------------------------------------------------------------------------
-- Updates

addTags :: Id
        -> [String]
        -> Update ()
addTags pid tags =
  forM_ tags $ \t ->
    updateSql_ "INSERT INTO tags (id, tag) VALUES (?, ?)"
               [ toSql pid, toSql t ]
