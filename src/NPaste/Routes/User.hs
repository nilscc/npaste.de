{-# LANGUAGE OverloadedStrings #-}

module NPaste.Routes.User
  ( userR
  , setupUserMenu
  ) where

import qualified Data.ByteString.Char8 as B8
import Data.Maybe
import Happstack.Server hiding (require, addCookie)

import NPaste.Database
import NPaste.State
import NPaste.Types
import NPaste.Html
import NPaste.Utils

userR :: NPaste ()
userR = do
  mu         <- getCurrentUser
  ActiveMenu .= M_User mu
  CSS        .= [ "user.css" ]
  choice
    [ nullDir >> choice   [ requireNoUser >> loginR
                          , requireUser  >>= profileR ]
    , dir "login"         $ requireNoUser >> loginR
    , dir "register"      $ requireNoUser >> registerR
    , dir "lost-password" $ requireNoUser >> lostPasswordR
    , dir "settings"      $ requireUser  >>= settingsR
    , dir "profile"       $ requireUser  >>= profileR
    ]

loginR :: NPaste ()
loginR = choice
  [ do methodM POST
       pdata <- getPostData
       choice
         [ do -- require that both email and password are correct
              u    <- require $ getUserByEmail  (getValue pdata "email")
              isOk <-           checkPassword u (getValue pdata "password")
              unless isOk mzero
              -- add session to DB
              rq <- askRq
              let ip = fromMaybe (fst $ rqPeer rq) $
                         getHeader' "x-forwarded-for" (rqHeaders rq)
                  ua = fromMaybe "" $
                         getHeader' "user-agent"      (rqHeaders rq)
              s  <- addSession (Just u) ip ua
              -- set session cookie
              let clife = Expires (sessionExpires s)
                  ck    = mkCookie "sessionId" (sessionId s)
              addCookie clife ck
              -- forward to index page
              ResponseCode .= seeOther ("/" :: String)
              HtmlBody     .= loginCorrectHtml
           -- throw error otherwise
         , HtmlBody .= loginHtml (Just "Wrong email or password.") pdata
         ]
  , HtmlBody .= loginHtml Nothing nullPData
  ]
 where
  getHeader' h = fmap B8.unpack . getHeader h

registerR :: NPaste ()
registerR = return ()

lostPasswordR :: NPaste ()
lostPasswordR = return ()

settingsR :: User -> NPaste ()
settingsR _ = return ()

profileR :: User -> NPaste ()
profileR _ = return ()


--------------------------------------------------------------------------------
-- User management

-- | Get the current session/user from the session cookie. This function should
-- probably be used only once (hence the awfully long name).
getCurrentUserFromSessionCookie :: NPaste (Maybe User)
getCurrentUserFromSessionCookie = fmap join . optional $ do
  s  <- lookCookieValue "sessionId"
  rq <- askRq
  let ip = fromMaybe (fst $ rqPeer rq) $
            getHeader' "x-forwarded-for" (rqHeaders rq)
      ua = fromMaybe "" $
            getHeader' "user-agent"      (rqHeaders rq)
  ms <- getSession s ip ua
  return $ join $ fmap sessionUser ms
 where
  getHeader' h = fmap B8.unpack . getHeader h

getCurrentUser :: NPaste (Maybe User)
getCurrentUser = unCurrentUser `fmap` gets currentUser

requireUser :: NPaste User
requireUser =
  maybe mzero return =<< getCurrentUser

requireNoUser :: NPaste ()
requireNoUser =
  maybe (return ()) (const mzero) =<< getCurrentUser

--------------------------------------------------------------------------------
-- Menu generation

setupUserMenu :: NPaste ()
setupUserMenu = do
  mu <- getCurrentUserFromSessionCookie
  case mu of
       Just u  -> do
         CurrentUser   .= Just u
         MenuStructure .= userMenu u
       Nothing ->
         MenuStructure .= anonMenu



--------------------------------------------------------------------------------
-- Post data

getPostData :: NPaste PostData
getPostData = do
  decodeBody (defaultBodyPolicy "/tmp/npaste.de/" 1000000 1000000 1000000)
  fmap PostData $ body lookPairs

nullPData :: PostData
nullPData = PostData []
