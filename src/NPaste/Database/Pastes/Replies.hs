{-# LANGUAGE NamedFieldPuns #-}

module NPaste.Database.Pastes.Replies
  ( -- * Queries
    getReplies
    -- * Updates
  , addReplies
  ) where

import Control.Monad
import Data.Maybe

import NPaste.Database.Connection
import NPaste.Database.Users
import NPaste.Types
import NPaste.Utils


--------------------------------------------------------------------------------
-- Queries

getReplies :: PasteInfo -> Query [ID]
getReplies pinfo = do
  rpi <- getReplies' pinfo
  forM rpi $ \PasteInfo{ p_id, p_user_id, p_id_is_global } ->
    if p_id_is_global then do
      return $ ID p_id
     else do
      u <- getUserById p_user_id
      return $ case u of
                    Just u' -> PrivateID u' p_id
                    _       -> ID p_id

getReplies' :: PasteInfo -> Query [PasteInfo]
getReplies' PasteInfo{ p_id, p_user_id } =
  fmap (catMaybes . map convertMaybe) $
       querySql "SELECT * FROM HS_PasteInfo \
                \WHERE (p_id, p_user_id) IN ( SELECT r_Paste_id, r_Paste_user_id \
                \                               FROM replies \
                \                              WHERE r_reply_Paste_id = ? AND r_reply_Paste_user_id = ? \
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
    updateSql_ "INSERT INTO replies (r_Paste_id, r_Paste_user_id, r_reply_Paste_id, r_reply_Paste_user_id) \
               \VALUES (?, ?, ?, ?)"
               [ toSql pid,  toSql (maybe (-1) u_id mu)
               , toSql mpid, toSql (maybe (-1) u_id mru) ]
