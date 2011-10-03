{-# LANGUAGE NamedFieldPuns #-}

module NPaste.Database.Pastes.Content
  ( -- * Queries
    getContent

    -- * Updates
  , addContent
  ) where

import Data.ByteString (ByteString)

import NPaste.Database.Connection
import NPaste.Types

--------------------------------------------------------------------------------
-- Queries

getContent :: ID
           -> Query (Maybe ByteString)
getContent pid =
  fmap (\c -> case c of
                   [[sql]] -> Just $ byteaUnpack sql
                   _       -> Nothing)
       (querySql "SELECT pc_content FROM Paste_contents \
                 \WHERE pc_Paste_id = ? AND pc_Paste_user_id = ?"
                 [toSql p_id, toSql p_uid])
 where
  (p_id,p_uid) = case pid of
                      ID                   i -> (i,-1)
                      PrivateID User{u_id} i -> (i,u_id)


--------------------------------------------------------------------------------
-- Updates

addContent :: ID
           -> ByteString
           -> Update ()
addContent pid t =
  fmap (const ())
       (updateSql "INSERT INTO Paste_contents (pc_Paste_id, pc_Paste_user_id, pc_content) \
                  \VALUES (?, ?, ?)"
                  [toSql p_id, toSql p_u, byteaPack t])
 where
  (p_id, p_u) = case pid of
                     ID                   pid' -> (pid',-1)
                     PrivateID User{u_id} pid' -> (pid',u_id)
