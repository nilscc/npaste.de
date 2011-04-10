module NPaste.Database.Users
  ( -- * Queries
    getUserById
  , getUserByName
  , getAllUsers
    -- * Updates
  ) where

import Data.Maybe

import NPaste.Database.Connection
import NPaste.Types
import NPaste.Utils


--------------------------------------------------------------------------------
-- Queries

getUserById :: Int -> Query (Maybe User)
getUserById uid = do
  fmap convertListToMaybe $
       querySql "SELECT * FROM HS_User WHERE u_id = ?"
                [toSql uid]

getUserByName :: String -> Query (Maybe User)
getUserByName name =
  fmap convertListToMaybe $
       querySql "SELECT * FROM HS_User WHERE u_name = ?"
                [toSql name]

getAllUsers :: Query [User]
getAllUsers = do
  fmap (catMaybes . map convertMaybe) $
       querySql "SELECT * FROM HS_User" []


--------------------------------------------------------------------------------
-- Updates
