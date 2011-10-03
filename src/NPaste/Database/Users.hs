{-# LANGUAGE NamedFieldPuns #-}

module NPaste.Database.Users
  ( -- ** Queries
    getUserByName
  , getUserById
  , checkPassword

    -- ** Updates
  , newUser
  , changePassword
  ) where

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
       querySql "SELECT * FROM HS_User WHERE u_id = ?"
                [toSql uid]

getUserByName :: String -> Query (Maybe User)
getUserByName ""   = return Nothing
getUserByName name =
  fmap convertListToMaybe $
       querySql "SELECT * FROM HS_User WHERE u_name = ?"
                [toSql name]

{-
getAllUsers :: Query [User]
getAllUsers = do
  fmap (catMaybes . map convertMaybe) $
       querySql "SELECT * FROM HS_User" []
-}

getNextId :: Query Int
getNextId = do
  res <- querySql "SELECT max(u_id)+1 FROM users" []
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
  let u = User { u_id = i, u_name = un, u_email = me }
  mpw <- packPassword pw
  case mpw of
       Nothing    -> throwError AUE_NoPassword
       Just sqlPw -> do
         handleSql sqlErrorToAUE $
           updateSql_ "INSERT INTO users (u_id, u_name, u_email, u_password) \
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
