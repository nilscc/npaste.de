{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Paste.Login.Logout
    ( logoutPerform
    ) where

import HSP
import Happstack.Server
import Happstack.State

import qualified Happstack.Auth as Auth

import Paste.View
import Paste.Types
import Users.State

--------------------------------------------------------------------------------
-- | Main logout page

logoutPerform :: ServerPart Response
logoutPerform = do

    login <- getLogin
    case login of

         LoggedInAs skey -> do
             addCookie 0 -- delete
                       (mkCookie "session-key" "")
             update $ RemoveSessionData skey
             update $ Auth.DelSession skey

             xmlResponse $ htmlBody NotLoggedIn [logoutHsp]

         NotLoggedIn -> xmlResponse $ htmlBody login [notLoggedInHsp]

logoutHsp :: HSP XML
logoutHsp =
    <div id="main">
        <h1>Logout successful</h1>
        <p>Your cookie and your session data got removed. Your logout was successful.</p>
    </div>

notLoggedInHsp :: HSP XML
notLoggedInHsp =
    <div id="main">
        <h1>Logout failed</h1>
        <p>You are currently not logged in.</p>
    </div>
