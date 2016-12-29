module NPaste.Database.Pastes.Replies
  ( -- * Queries
    getReplyIds
    -- * Updates
  , addReplyIds
  ) where

import Control.Monad

import NPaste.Database.Connection
import NPaste.Types
import NPaste.Utils


--------------------------------------------------------------------------------
-- Queries

getReplyIds :: Id -> Query [Id]
getReplyIds pid =
  fmap convertToList $
       querySql "SELECT reply_id FROM replies WHERE paste_id = ?"
                [ toSql pid ]


--------------------------------------------------------------------------------
-- Updates

addReplyIds :: Id
            -> [Id]
            -> Update ()
addReplyIds rply pids =
  forM_ pids $ \pid ->
    updateSql_ "INSERT INTO replies (paste_id, reply_id) VALUES (?, ?)"
               [ toSql pid, toSql rply ]
