{-# LANGUAGE NamedFieldPuns #-}

module NPaste.Database.Users
  ( -- ** Queries
    getUserByName
  , getUserByEmail
  , getUserById
  -- , getAllUsers
  , checkPassword

    -- ** Updates
  , newUser
  , changePassword
  ) where

-- import Data.Maybe
import Database.HDBC.PostgreSQL

import NPaste.Database.Connection
import NPaste.Database.Users.Password
import NPaste.Types
import NPaste.Utils


--------------------------------------------------------------------------------
-- Queries

getUserById :: Int -> Query (Maybe User)
getUserById (-1) = return Nothing
getUserById uid  =
  fmap convertListToMaybe $
       querySql "SELECT id, name, email, default_hidden FROM users WHERE id = ?"
                [toSql uid]

getUserByEmail :: String -> Query (Maybe User)
getUserByEmail "" = return Nothing
getUserByEmail email  =
  fmap convertListToMaybe $
       querySql "SELECT id, name, email, default_hidden FROM users WHERE email = ?"
                [toSql email]

getUserByName :: String -> Query (Maybe User)
getUserByName ""   = return Nothing
getUserByName name =
  fmap convertListToMaybe $
       querySql "SELECT id, name, email, default_hidden FROM users WHERE name = ?"
                [toSql name]

{-
getAllUsers :: Query [User]
getAllUsers = do
  fmap (catMaybes . map convertMaybe) $
       querySql "SELECT id, name, password, email, default_hidden FROM users" []
-}

getNextId :: Query Int
getNextId = do
  res <- querySql "SELECT max(id)+1 FROM users" []
  case res of
       [[i]] -> return $ fromSql i
       _     -> return 0


--------------------------------------------------------------------------------
-- Updates

newUser :: String           -- ^ username
        -> String           -- ^ password (plaintext)
        -> Maybe String     -- ^ optional email
        -> Update (Either AddUserError User)
newUser un pw me = runErrorT $ do
  unless (all (`elem` validChars) un) $
    throwError $ AUE_InvalidUsername un
  i <- getNextId
  let u = User i un me False
  mpw <- packPassword pw
  case mpw of
       Nothing    -> throwError AUE_NoPassword
       Just sqlPw -> do
         handleSql sqlErrorToAUE $
           updateSql_ "INSERT INTO users (id, name, email, password) \
                      \VALUES (?, ?, ?, ?)"
                      [ toSql i, toSql un, toSql me, sqlPw ]
         return u

validChars :: [Char]
validChars = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']

sqlErrorToAUE :: SqlError -> AddUser ()
sqlErrorToAUE e =
  case seState e of
       l | l == uniqueViolation ->
           throwError $ AUE_AlreadyExists
         | otherwise ->
           throwError $ AUE_Other (show e)
