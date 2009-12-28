{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
module Users.Auth
    ( registerH
    , loginH
    , getSessionId
    , setSessionCookie
    , clearSessionCookie
    ) where

import Happstack.Server
import Happstack.State      (update, query)
import Happstack.Util.Common       (Seconds)

import Control.Monad        (liftM, mplus)
import Data.Maybe           (fromMaybe)

import Users.State

--------------------------------------------------------------------------------
-- Pure stuff
--------------------------------------------------------------------------------

validChars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

data PostUser = JustUser User
              | InvalidName
              | InvalidPassword

-- nice helper for FromData instance definitions
optLook s = look s `mplus` return ""

-- | Get post data
instance FromData PostUser where
    fromData = do

        name    <- optLook "name"
        pass    <- optLook "pass"
        email   <- optLook "email"

        let email' = if null email
                        then Nothing
                        else Just email
            response | null name
                       || length name >= 20
                       || not (all (`elem` validChars) name) = InvalidName
                     | null pass
                       || length pass >= 20 = InvalidPassword
                     | otherwise = JustUser $ User name
                                                   (passwordFromString pass)
                                                   email'
        return response

instance FromData (Login,Password) where
    fromData = do
        login <- look "login"
        pass  <- optLook "password"
        return (login, passwordFromString pass)



--------------------------------------------------------------------------------
-- Cookies!
--------------------------------------------------------------------------------

sessionCookie = "SessionID"

-- | Set session cookie
setSessionCookie :: (FilterMonad Response m)
                 => Seconds
                 -> SessionID
                 -> m ()
setSessionCookie timeout = addCookie timeout . mkCookie sessionCookie . show

-- | Clear session cookie
clearSessionCookie :: (FilterMonad Response m) => m ()
clearSessionCookie = addCookie 0 $ mkCookie sessionCookie "0"

-- | Get session ID from cookie
getSessionId :: RqData SessionID
getSessionId = readCookieValue sessionCookie


--------------------------------------------------------------------------------
-- Register, log in and out
--------------------------------------------------------------------------------

-- | Register a new user
registerH :: (UserReply -> ServerPartT IO Response) -- ^ handle success, response from AddUser
          -> (PostUser  -> ServerPartT IO Response) -- ^ handle failure, response from FromData
          -> ServerPartT IO Response
registerH successH errorH = do
    methodM POST
    user <- getData
    case user of
         Just (JustUser u) -> do uReply <- update $ AddUser u
                                 successH uReply
         other -> errorH $ fromMaybe InvalidName other


-- | Login by login & name
loginH :: (SessionID -> ServerPartT IO Response)   -- ^ handle success
       -> (UserReply -> ServerPartT IO Response)   -- ^ handle failure, response from Validate
       -> ServerPartT IO Response
loginH successH errorH = do
    -- get POST data
    methodM POST
    pData <- getData
    -- rq for host information
    rq    <- askRq

    -- return error if Nothing is returned by getData
    maybe (errorH WrongLogin) `flip` pData $ \(name,pw) -> do

        -- validate user
        valid <- query $ Validate name pw
        case valid of
             OK user -> do sid <- update $ LoginUser user (rqPeer rq)
                           setSessionCookie 2678400 sid -- one month
                           successH sid
             other   -> errorH other

-- | Logout user
logoutH :: ServerPartT IO Response  -- ^ success handle
        -> ServerPartT IO Response  -- ^ error handle
        -> ServerPartT IO Response
logoutH success error = do
    sid <- getDataFn getSessionId
    maybe error `flip` sid $ \sid -> do
        update $ LogoutUser sid
        clearSessionCookie
        success
