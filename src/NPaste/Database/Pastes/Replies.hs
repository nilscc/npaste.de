{-# LANGUAGE NamedFieldPuns, ViewPatterns #-}

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
import NPaste.Types.Database
import NPaste.Utils


--------------------------------------------------------------------------------
-- Queries

getReplies :: ID -> Query [ID]
getReplies pid = do
  rpi <- getReplies' pid
  forM rpi $ \PasteInfo{ p_id, p_user_id, p_id_is_global } ->
    if p_id_is_global then do
      return $ ID p_id
     else do
      u <- getUserById p_user_id
      return $ case u of
                    Just u' -> PrivateID u' p_id
                    _       -> ID p_id

getReplies' :: ID -> Query [PasteInfo]
getReplies' pid =
  fmap (catMaybes . map convertMaybe) $
       querySql "SELECT * FROM HS_PasteInfo \
                \WHERE (p_id, p_user_id) IN ( SELECT r_Paste_id, r_Paste_user_id \
                \                               FROM replies \
                \                              WHERE r_reply_Paste_id = ? AND r_reply_Paste_user_id = ? \
                \                           )"
                [ toSql p_id, toSql p_user_id ]
 where
  (p_id, p_user_id) = case pid of
                           ID                   i -> (i, -1)
                           PrivateID User{u_id} i -> (i, u_id)


--------------------------------------------------------------------------------
-- Updates

addReplies :: ID
           -> [ID]
           -> Update ()
addReplies pId rpls =
  forM_ rpls $ \(idToTuple->(r_pid, r_u)) ->
    updateSql_ "INSERT INTO replies (r_Paste_id, r_Paste_user_id, r_reply_Paste_id, r_reply_Paste_user_id) \
               \VALUES (?, ?, ?, ?)"
               [ toSql p_pid, toSql p_u
               , toSql r_pid, toSql r_u ]
 where
  idToTuple (ID pid)                   = (pid,-1)
  idToTuple (PrivateID User{u_id} pid) = (pid,u_id)
  (p_pid, p_u) = idToTuple pId
