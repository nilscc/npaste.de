{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module NPaste.Database.Users
  ( -- * Queries
    getUserByName
  , getUserByEmail
  , getUserById
  -- , getAllUsers
  , checkPassword

    -- * Updates
  , addUser
  , addIncativeEmail
  , activateEmail
  , updateUserPassword
  , updateUserSettings
    -- ** Inactive users
  , addInactiveUser
  , rmInactiveUser
    -- ** Lost passwords
  , addLostPasswordKey
  , changeLostPassword
  ) where

-- import Data.Maybe
-- import Control.Monad.Trans.Except
import Data.Time
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
       querySql "SELECT id, name, email, default_hidden, public_profile FROM active_users WHERE id = ?"
                [toSql uid]

getUserByEmail :: String -> Query (Maybe User)
getUserByEmail "" = return Nothing
getUserByEmail email  =
  fmap convertListToMaybe $
       querySql "SELECT id, name, email, default_hidden, public_profile FROM active_users WHERE email = ?"
                [toSql email]

getUserByName :: String -> Query (Maybe User)
getUserByName ""   = return Nothing
getUserByName name =
  fmap convertListToMaybe $
       querySql "SELECT id, name, email, default_hidden, public_profile FROM active_users WHERE name = ?"
                [toSql name]

getNextId :: Query Int
getNextId = do
  res <- querySql "SELECT max(id)+1 FROM users" []
  case res of
       [[i]] -> return $ fromSql i
       _     -> return 0


--------------------------------------------------------------------------------
-- Updates

addUser :: String           -- ^ username
        -> String           -- ^ password (plaintext)
        -> Maybe String     -- ^ optional email
        -> Update (Either AddUserError User)
addUser un pw me = runWithEither $ do
  unless (all (`elem` validChars) un) $
    throwError $ AUE_InvalidUsername un
  i <- lift $ liftQuery getNextId
  let u = User i un me False True
  mpw <- packPassword pw
  case mpw of
       Nothing    -> throwError AUE_NoPassword
       Just sqlPw -> do
         handleSql (return . sqlErrorToAUE) $
           updateSql_ "INSERT INTO users (id, name, email, password) \
                      \VALUES (?, ?, ?, ?)"
                      [ toSql i, toSql un, toSql me, sqlPw ]
         return u

validChars :: [Char]
validChars = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']

sqlErrorToAUE :: SqlError -> AddUserError
sqlErrorToAUE e =
  case seState e of
       l | l == uniqueViolation -> do
           AUE_AlreadyExists
         | otherwise ->
           AUE_Other (show e)

updateUserPassword :: User -> String -> Update ()
updateUserPassword u p = changePassword u p >> return ()

addIncativeEmail :: User
                 -> String         -- ^ new email
                 -> String         -- ^ activation key
                 -> Update Bool    -- ^ true on success, false on UNIQUE violation/other
addIncativeEmail User{ userId } email akey = runWithId $
  handleSql onSqlError $ do
    rmOldInactiveEmails
    -- make sure the email really is unique
    c <- liftQuery . fmap convertListToMaybe $
              querySql "SELECT count(*) \
                       \  FROM users u LEFT JOIN new_emails ne ON u.id = ne.user_id \
                       \ WHERE u.email = ?"
                       [ toSql email ]
    if c == Just (0 :: Int) then do
       now <- liftIO getCurrentTime
       let expires = addUTCTime (60 * 60 * 24 * 7) now
       updateSql_ "INSERT INTO new_emails(user_id, activation_key, email, expires) \
                  \     VALUES           (?      , ?             , ?    , ?      ) "
                  [ toSql userId, toSql akey, toSql email, toSql expires ]
       return True
     else
       return False
 where
  onSqlError _ = return False

activateEmail :: User
              -> String                 -- ^ activation key
              -> Update (Maybe String)  -- ^ (Just newEmail) on success
activateEmail User{ userId } akey = runWithId $
  handleSql onSqlError $ do
    rmOldInactiveEmails
    res <- liftQuery . fmap convertListToMaybe $
                querySql "SELECT email FROM new_emails \
                         \ WHERE user_id = ? AND activation_key = ?"
                         [ toSql userId, toSql akey ]
    maybe (return Nothing) `flip` res $ \(email :: String) -> do
      updateSql_ "UPDATE users SET email = ? WHERE id = ?"
                 [ toSql email, toSql userId ]
      updateSql_ "DELETE FROM new_emails WHERE user_id = ?"
                 [ toSql userId ]
      return $ Just email
 where
  onSqlError _ = return Nothing

rmOldInactiveEmails :: Update ()
rmOldInactiveEmails =
  updateSql_ "DELETE FROM new_emails WHERE expires < now ()" []


updateUserSettings :: User -> Update ()
updateUserSettings u =
  updateSql_ "UPDATE users SET default_hidden = ?, public_profile = ?\
             \ WHERE id = ?"
             [ toSql (userDefaultHidden u), toSql (userPublicProfile u)
             , toSql (userId u) ]

addLostPasswordKey :: User
                   -> String       -- ^ the key
                   -> Update ()
addLostPasswordKey u k = runWithId $ do
  now <- liftIO getCurrentTime
  let expires = addUTCTime (60 * 60 * 24 * 7) now
  handleSql (\_ -> return ()) $
    updateSql_ "INSERT INTO lost_password_keys(user_id, key, expires)\
               \     VALUES                   (?      , ?  , ?      )"
               [ toSql (userId u), toSql k, toSql expires ]

changeLostPassword :: User
                   -> String        -- ^ the key
                   -> String        -- ^ the new password
                   -> Update Bool
changeLostPassword u k pw = do
  -- remove expired passwords
  updateSql_ "DELETE FROM lost_password_keys WHERE expires < now()" []
  r <- updateSql "DELETE FROM lost_password_keys \
                 \ WHERE user_id = ? AND key = ?"
                 [ toSql (userId u), toSql k ]
  if r == 1 then do
     pwr <- changePassword u pw
     -- clean up any other password requests
     updateSql_ "DELETE FROM lost_password_keys WHERE user_id = ?" [ toSql (userId u) ]
     return pwr
   else
     return False


--------------------------------------------------------------------------------
-- Inactive users

addInactiveUser :: User
                -> String     -- ^ activation key
                -> Update ()
addInactiveUser User{ userId } akey = do
  rmOldInactiveUsers
  now <- liftIO getCurrentTime
  let expires = addUTCTime (60 * 60 * 24 * 7) now
  updateSql_ "INSERT INTO inactive_users (user_id, activation_key, expires) \
             \     VALUES                (?      , ?             , ?      ) "
             [toSql userId, toSql akey, toSql expires]

rmInactiveUser :: Int             -- ^ user id
               -> String          -- ^ activation key
               -> Update Bool     -- ^ true on success
rmInactiveUser userId akey = do
  rmOldInactiveUsers
  r <- updateSql "DELETE FROM inactive_users \
                 \      WHERE user_id = ? AND activation_key = ? "
                 [toSql userId, toSql akey]
  return $ r == 1

rmOldInactiveUsers :: Update ()
rmOldInactiveUsers =
  updateSql_ "DELETE FROM inactive_users WHERE expires < now()" []
