{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeOperators, ScopedTypeVariables
    #-}
module Users.State
    ( Validate (..)
    , Validate' (..)
    , UserExists (..)

    , AddUser (..)
    , AddUser' (..)
    , RemoveUser (..)
    , RemoveUser' (..)

    , LoginUser (..)
    , LogoutUser (..)

    , Login
    , Password
    , PasswordPlain
    , Email

    , passwordFromString

    , module Users.State.User
    , module Users.State.Users
    , module Users.State.UserReply
    , module Users.State.SessionID
    ) where

import Users.State.User
import Users.State.Users
import Users.State.UserReply
import Users.State.SessionID

import Happstack.Data
import Happstack.State
import Happstack.Crypto.MD5         (md5)
import Happstack.Server.HTTP.Types  (Host)

import Data.List                    (find, delete)
import Control.Monad                (liftM)
import Control.Monad.State          (modify)
import Control.Monad.Reader         (ask)

import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Map                   as M



--------------------------------------------------------------------------------
-- Pure stuff
--------------------------------------------------------------------------------

type Login          = String
type PasswordPlain  = String
type Password       = B.ByteString
type Email          = String

-- | MD5 generation
passwordFromString :: String -> Password
passwordFromString = B.concat . C.toChunks . md5 . C.pack


third (a,b,c) = c


--------------------------------------------------------------------------------
-- Password/user validation
--------------------------------------------------------------------------------

-- | Validate a user + password
validate :: Login
         -> Password
         -> Query Users UserReply
validate login pw = do
    users' <- ask
    case find ((== login) . userLogin) (allUsers users') of
         Nothing -> return WrongLogin
         Just us -> return $ if userPassword us == pw
                                then OK us
                                else WrongPassword

-- | Validate a user, plain password version
validate' :: Login
          -> PasswordPlain
          -> Query Users UserReply
validate' name pw = validate name (passwordFromString pw)


-- | Check if a login already exists
userExists :: Login
           -> Query Users Bool
userExists name = ask >>= return . elem name . map userLogin . allUsers



--------------------------------------------------------------------------------
-- Add/remove users
--------------------------------------------------------------------------------

-- | Add new user
addUser :: User
        -> Update Users UserReply
addUser user = do
    exists <- runQuery . userExists $ userLogin user
    if exists
       then return $ AlreadyExists
       else do modify  $ \users -> users { allUsers = user : allUsers users }
               return  $ OK user

-- | Plaintext version of addUser
addUser' :: Login
         -> PasswordPlain
         -> Maybe Email
         -> Update Users UserReply
addUser' name pw email = addUser $ User name (passwordFromString pw) email


-- | Remove a user
removeUser :: User
           -> Update Users ()
removeUser user = modify $ \userList -> userList { allUsers = delete user $ allUsers userList }


-- | Plaintext version of removeUser
removeUser' :: Login
            -> PasswordPlain
            -> Maybe Email
            -> Update Users ()
removeUser' name pw email = removeUser $ User name (passwordFromString pw) email



--------------------------------------------------------------------------------
-- Login/logout users
--------------------------------------------------------------------------------

-- generate a new sessionID
generateSessionID :: Query Users SessionID
generateSessionID = do
    users <- ask
    sid   <- liftM abs getRandom >>= return . SessionID
    case sid `M.lookupIndex` sessionIDs users of
         Nothing -> return sid
         Just _  -> generateSessionID

-- | Log in a user
loginUser :: User   -- ^ user
          -> Host   -- ^ (IP :: String, Port :: Integer) pair
          -> Update Users SessionID
loginUser user host = do
    sid <- runQuery $ generateSessionID
    now <- getEventClockTime
    modify $ \ users -> users { sessionIDs = M.insert sid (user,host,now) $ sessionIDs users }
    return sid

-- | Log out a user
logoutUser :: SessionID
           -> Update Users ()
logoutUser sid = modify $ \ users -> users { sessionIDs = M.delete sid $ sessionIDs users }



--------------------------------------------------------------------------------
-- Register methods
--------------------------------------------------------------------------------

$(mkMethods ''Users [ 'validate
                    , 'validate'
                    , 'userExists

                    , 'addUser
                    , 'addUser'
                    , 'removeUser
                    , 'removeUser'

                    , 'loginUser
                    , 'logoutUser
                    ])
