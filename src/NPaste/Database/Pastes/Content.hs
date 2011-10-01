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

getContent :: Maybe User
           -> String
           -> Query (Maybe ByteString)
getContent mu pid =
  fmap (\c -> case c of
                   [[sql]] -> Just $ byteaUnpack sql
                   _       -> Nothing)
       (querySql "SELECT pc_content FROM Paste_contents \
                 \WHERE pc_Paste_id = ? AND pc_Paste_user_id = ?"
                 [toSql pid, toSql (maybe (-1) u_id mu)])


--------------------------------------------------------------------------------
-- Updates

addContent :: Maybe User
           -> String
           -> ByteString
           -> Update ()
addContent mu pid t =
  fmap (const ())
       (updateSql "INSERT INTO Paste_contents (pc_Paste_id, pc_Paste_user_id, pc_content) \
                  \VALUES (?, ?, ?)"
                  [toSql pid, toSql (maybe (-1) u_id mu), byteaPack t])
