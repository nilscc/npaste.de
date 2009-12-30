{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeOperators, ScopedTypeVariables,
    TypeSynonymInstances
    #-}
module Users.State
    ( Validate (..)
    , Validate' (..)
    , UserExists (..)

    , AddUser (..)
    , AddUser' (..)
    , RemoveUser (..)
    , RemoveUser' (..)
    , RemoveInactiveUsers (..)

    , AddInactiveUser (..)
    , ActivateUser (..)

    , LoginUser (..)
    , LogoutUser (..)
    , UserOfSessionId (..)
    , UserOfLogin (..)

    , GetAllUsers (..)

    , AddRegisteredHost (..)
    , ClearRegisteredHosts (..)

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
import Happstack.Util.Mail          (NameAddr (..))
import Happstack.State.ClockTime    (ClockTime (..))

import System.Time                  (diffClockTimes, noTimeDiff, TimeDiff (..), normalizeTimeDiff)

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
type ActivationKey  = String

-- | MD5 generation
passwordFromString :: String -> Password
passwordFromString = B.concat . C.toChunks . md5 . C.pack

-- moep
third (a,b,c) = c

-- Alpha num
alphaNum = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9']

ulogin (User login _ _)           = login
ulogin (InactiveUser login _ _ _) = login


--------------------------------------------------------------------------------
-- Password/user validation
--------------------------------------------------------------------------------

-- | Validate a user + password
validate :: Login
         -> Password
         -> Query Users UserReply
validate login pw = do
    users' <- ask
    case find f (allUsers users') of
         Nothing -> return WrongLogin
         Just us -> return $ if userPassword us == pw
                                then OK us
                                else WrongPassword
  where f (User name _ _) = login == name
        f (InactiveUser _ _ _ _) = False

-- | Validate a user, plain password version
validate' :: Login
          -> PasswordPlain
          -> Query Users UserReply
validate' name pw = validate name (passwordFromString pw)


-- | Check if a login already exists
userExists :: Login
           -> Query Users Bool
userExists name = ask >>= return . or . map step . allUsers
  where step (User login _ _)         = login == name
        step (InactiveUser login _ _ _) = login == name


--------------------------------------------------------------------------------
-- Registration / Activation
--------------------------------------------------------------------------------

-- Helper :)
randomString :: Int -> AnyEv String
randomString l
    | l >= 1 = do
        ints <- mapM (const $ getRandomR (0,length alphaNum)) [1..l]
        return $ map ((!!) (cycle alphaNum) . abs) ints
    | otherwise   = return ""

-- | Add inactive user, return activation key on success or the UserReply on failure
addInactiveUser :: Login
                -> Email
                -> Update Users UserReply
addInactiveUser login email = do
    users   <- ask
    let allActivationKeys k (InactiveUser _ _ akey _) rest | akey == k = akey : rest
        allActivationKeys _ _ rest = rest
        genkey = do k <- randomString 30
                    case foldr (allActivationKeys k) [] (allUsers users) of
                         [] -> return k
                         _  -> genkey
    akey    <- genkey
    now     <- getEventClockTime
    addUser $ InactiveUser login email akey now

-- | Activate an inactive user, return a random password as plain text.
-- Warning: No validation whether the inactive user is too old.
activateUser :: User
             -> ActivationKey
             -> Update Users (Maybe PasswordPlain)
activateUser iu@(InactiveUser login email akey ctime) akey' | akey == akey' = do
    -- remove inactive user
    removeUser iu
    -- add active user
    pwd    <- randomString 8
    ureply <- addUser $ User login (passwordFromString pwd) email
    -- return plaintext
    return $ case ureply of
                  OK _ -> Just pwd
                  _    -> Nothing

activateUser _ _ = return Nothing



--------------------------------------------------------------------------------
-- Add/remove users
--------------------------------------------------------------------------------

-- | Add new user
addUser :: User
        -> Update Users UserReply
addUser u@(User login _ _)               | all (`elem` alphaNum) login = addUserHelper u
addUser u@(InactiveUser login email _ _) | all (`elem` alphaNum) login = addUserHelper u
addUser _ = return WrongLogin
addUserHelper user = do
    exists <- runQuery . userExists $ ulogin user
    if exists
       then return $ AlreadyExists
       else do modify  $ \users -> users { allUsers = user : allUsers users }
               return  $ OK user

-- | Plaintext version of addUser
addUser' :: Login
         -> PasswordPlain
         -> Email
         -> Update Users UserReply
addUser' name pw email = addUser $ User name (passwordFromString pw) email


-- | Remove a user
removeUser :: User
           -> Update Users ()
removeUser user = modify $ \userList -> userList { allUsers = delete user $ allUsers userList }


-- | Plaintext version of removeUser
removeUser' :: Login
            -> PasswordPlain
            -> Email
            -> Update Users ()
removeUser' name pw email = removeUser $ User name (passwordFromString pw) email


-- | Remove all inactive users older than two days
removeInactiveUsers :: Update Users ()
removeInactiveUsers = do
    users <- ask
    now   <- getEventClockTime

    let notTooOld (User _ _ _) = True
        notTooOld (InactiveUser _ _ _ ctime) = diffClockTimes now ctime <= noTimeDiff { tdDay = 2 }

    modify $ \users -> users { allUsers = filter notTooOld $ allUsers users }

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

-- | Get user of a session ID
userOfSessionId :: SessionID
                -> Host
                -> Query Users (Maybe User)
userOfSessionId sid host = do
    users <- ask
    case sid `M.lookup` sessionIDs users of
         Just (u,h,_) | h == host -> return $ Just u
         _ -> return Nothing

-- | Get a user by his login name
userOfLogin :: Login -> Query Users (Maybe User)
userOfLogin login = do
    users <- ask
    return $ case find ((login ==) . ulogin) (allUsers users) of
                  Just user -> Just user
                  _ -> Nothing


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
-- Remembering & anti spam protection
--------------------------------------------------------------------------------

-- | Add a registered host, overwrite current ClockTime if host is already known
addRegisteredHost :: Host
                  -> Update Users ()
addRegisteredHost host = do
    users <- ask
    now   <- getEventClockTime
    modify $ \users -> users { registeredHosts = M.insert host now $ registeredHosts users }

-- | Remove all registered hosts older than X hours
clearRegisteredHosts :: Int -- ^ age in hours
                     -> Update Users ()
clearRegisteredHosts hour = do
    paste <- ask
    ctime <- getEventClockTime
    let maxTime = normalizeTimeDiff $ noTimeDiff { tdHour = hour }
        timeDiff _ time = let tdiff = normalizeTimeDiff $ diffClockTimes ctime time
                          in tdiff <= maxTime
    modify $ \paste -> paste { registeredHosts = M.filterWithKey timeDiff $ registeredHosts paste }


--------------------------------------------------------------------------------
-- Other stuff
--------------------------------------------------------------------------------

-- | Get all users
getAllUsers :: Query Users [User]
getAllUsers = ask >>= return . allUsers


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
                    , 'removeInactiveUsers

                    , 'addInactiveUser
                    , 'activateUser

                    , 'loginUser
                    , 'logoutUser
                    , 'userOfSessionId
                    , 'userOfLogin

                    , 'getAllUsers

                    , 'addRegisteredHost
                    , 'clearRegisteredHosts
                    ])
