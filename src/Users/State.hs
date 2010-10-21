{-# LANGUAGE TemplateHaskell, TypeFamilies, DeriveDataTypeable,
    FlexibleInstances, MultiParamTypeClasses, FlexibleContexts,
    UndecidableInstances, TypeOperators, ScopedTypeVariables,
    TypeSynonymInstances
    #-}
module Users.State
    ( AddInactiveUser (..)
    , RemoveInactiveUsers (..)
    , RemoveInactiveUser (..)
    , AddUser (..)

    , AddRegisteredHost (..)
    , ClearRegisteredHosts (..)

    , AskUsers (..)
    , AskUserData (..)
    , UserDataByUserId (..)
    , AskSessionData (..)

    , SetSessionData (..)
    , RemoveSessionData (..)

    , SetDefaultPasteSetting (..)
    , SetRequestedEmail (..)
    , SetNewEmail (..)

    , module Users.State.Users
    , module Users.State.UserData
    , module Users.State.PasteSettings
    ) where

import Control.Monad.State
import Control.Monad.Reader
import Happstack.State
import Happstack.Server.Internal.Types
import System.Time

import qualified Data.Map                   as M
import qualified Happstack.Auth             as Auth

import Users.State.Users
import Users.State.UserData
import Users.State.PasteSettings


type Login          = String
type Email          = String
type ActivationKey  = String
-- type InactiveUser   = (Login, Email)
-- type ErrorMessage   = String

--------------------------------------------------------------------------------
-- Usefull stuff/Helpers
--------------------------------------------------------------------------------

-- Alpha num
alphaNum :: [Char]
alphaNum = ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9']

-- Helper :)
randomString :: Int -> AnyEv String
randomString l
    | l >= 1 = do
        ints <- mapM (const $ getRandomR (0,length alphaNum)) [1..l]
        return $ map ((!!) (cycle alphaNum) . abs) ints
    | otherwise   = return ""

--------------------------------------------------------------------------------
-- Registration / Activation
--------------------------------------------------------------------------------

-- | Add inactive user, return activation key on success or the UserReply on failure
addInactiveUser :: Login
                -> Email
                -> Update Users (Maybe ActivationKey)
addInactiveUser login email = do

    inactives <- asks inactiveUsers

    let
        -- generate a random, 30 chars long activation key
        generateKey = do k <- randomString 30
                         if M.notMember k inactives
                            then return k
                            else generateKey

        -- Check if the user is already in the inactive user list
        inactive    = not . M.null $ M.filter (\(l, _, _) -> login == l) inactives

    if inactive
       then return Nothing
       else do now   <- getEventClockTime
               key   <- generateKey

               -- Add the inactive user to our list
               modify $ \q@Users { inactiveUsers = iu } -> q { inactiveUsers = M.insert key (login, email, now) iu }
               -- Return the activation key
               return $ Just key


-- | Remove all inactive users older than two days
removeInactiveUsers :: Update Users ()
removeInactiveUsers = do

    -- users <- ask
    ctime <- getEventClockTime

    let notTooOld (_,_,time) = let tdiff = normalizeTimeDiff $ diffClockTimes ctime time
                                   maxTime = normalizeTimeDiff $ noTimeDiff { tdDay = 2 }
                               in tdiff <= maxTime

    modify $ \users -> users { inactiveUsers = M.filter notTooOld $ inactiveUsers users }


-- | Remove an inactive user from the inactiveUsers list if the activation key
-- is valid.
removeInactiveUser :: String                        -- ^ User name
                   -> ActivationKey
                   -> Update Users (Maybe String)   -- ^ Returns the email address on success
removeInactiveUser login akey = do

    inactives <- asks inactiveUsers

    case M.lookup akey inactives of
         Just (l,email,_) | l == login -> do

             modify $ \u -> u { inactiveUsers = M.delete akey $ inactiveUsers u }
             return $ Just email

         _ -> return Nothing


-- | Add an active user with its user details
addUser :: Auth.UserId -> UserData -> Update Users ()
addUser id ud = modify $ \u -> u { userData = M.insert id ud $ userData u }



--------------------------------------------------------------------------------
-- Remembering & anti spam protection
--------------------------------------------------------------------------------

-- | Add a registered host, overwrite current ClockTime if host is already known
addRegisteredHost :: Host
                  -> Update Users ()
addRegisteredHost host = do
    -- users <- ask
    now   <- getEventClockTime
    modify $ \users -> users { registeredHosts = M.insert host now $ registeredHosts users }

-- | Remove all registered hosts older than X hours
clearRegisteredHosts :: Int -- ^ age in hours
                     -> Update Users ()
clearRegisteredHosts hour = do
    -- paste <- ask
    ctime <- getEventClockTime
    let maxTime = normalizeTimeDiff $ noTimeDiff { tdHour = hour }
        timeDiff time = let tdiff = normalizeTimeDiff $ diffClockTimes ctime time
                        in tdiff <= maxTime
    modify $ \paste -> paste { registeredHosts = M.filter timeDiff $ registeredHosts paste }


--------------------------------------------------------------------------------
-- Queries
--------------------------------------------------------------------------------

askUsers :: Query Users Users
askUsers = ask

askUserData :: Query Users (M.Map Auth.UserId UserData)
askUserData = asks userData

userDataByUserId :: Auth.UserId -> Query Users (Maybe UserData)
userDataByUserId uid = M.lookup uid `fmap` asks userData

askSessionData :: Query Users (M.Map Auth.SessionKey (Host, ClockTime))
askSessionData = asks sessionData


--------------------------------------------------------------------------------
-- Sessions
--------------------------------------------------------------------------------

setSessionData :: Auth.SessionKey -> (Host, ClockTime) -> Update Users ()
setSessionData skey (host, ctime) =
    modify $ \u -> u { sessionData = M.insert skey (host,ctime) $ sessionData u }

removeSessionData :: Auth.SessionKey -> Update Users ()
removeSessionData skey = modify $ \ u -> u { sessionData = M.delete skey $ sessionData u }


--------------------------------------------------------------------------------
-- Changing user settings
--------------------------------------------------------------------------------

setDefaultPasteSetting :: Auth.UserId -> PasteSettings -> Update Users ()
setDefaultPasteSetting uid pset =

    modify $ \u -> let f (Just ud) = Just ud { defaultPasteSettings = pset }
                       f ud        = ud
                   in u { userData = M.alter f uid $ userData u }

setRequestedEmail :: Auth.UserId
                  -> String         -- ^ new email
                  -> String         -- ^ activation key
                  -> Update Users ()
setRequestedEmail uid newEmail akey =

    modify $ \u -> let f (Just ud) = Just ud { userEmailRequested = Just (newEmail, akey) }
                       f ud        = ud
                   in u { userData = M.alter f uid $ userData u }

setNewEmail :: Auth.UserId
            -> Maybe String           -- ^ activation key
            -> Update Users Bool
setNewEmail uid Nothing = do

    ud <- asks userData
    case M.lookup uid ud of

         Just _ -> do
             modify $ \u -> let f (Just ud) = Just ud { userEmailRequested = Nothing }
                                f ud        = ud
                            in u { userData = M.alter f uid $ userData u }
             return True
         _ -> return False

setNewEmail uid (Just akey) = do

    ud <- asks userData
    case M.lookup uid ud of

         Just UserData { userEmailRequested = Just (_,activationKey) } | activationKey == akey -> do
             modify $ \u -> let f (Just ud@UserData { userEmailRequested = Just (newEmail, activationKey) }) | activationKey == akey =
                                    Just ud { userEmail = newEmail
                                            , userEmailRequested = Nothing
                                            }

                                f ud = ud
                            in u { userData = M.alter f uid $ userData u }
             return True

         _ -> return False

--
--------------------------------------------------------------------------------
-- Register methods
--------------------------------------------------------------------------------

$(mkMethods ''Users [ 'addInactiveUser
                    , 'removeInactiveUsers
                    , 'removeInactiveUser
                    , 'addUser

                    , 'addRegisteredHost
                    , 'clearRegisteredHosts

                    , 'askUsers
                    , 'askUserData
                    , 'userDataByUserId
                    , 'askSessionData

                    , 'setSessionData
                    , 'removeSessionData

                    , 'setDefaultPasteSetting
                    , 'setRequestedEmail
                    , 'setNewEmail
                    ])
