{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module NPaste.Routes.User
  ( userR
  , setupUserMenu
  ) where

import qualified Data.ByteString.Char8 as B8
import Data.Maybe
import Happstack.Server hiding (require, addCookie, expireCookie)
import System.Random

import NPaste.Database
import NPaste.State
import NPaste.Types
import NPaste.Html
import NPaste.Utils
import NPaste.Parser

import NPaste.Routes.View

userR :: NPaste ()
userR = do
  mu         <- getCurrentUser
  ActiveMenu .= Just $ M_User mu
  CSS        .= [ "user.css" ]
  choice
    [ nullDir >> choice   [ requireNoUser >> loginR
                          , requireUser  >>= profileR ]
    , dir "login"         $ requireNoUser >> loginR
    , dir "logout"        $ requireUser  >>= logoutR
    , dir "register"      $ requireNoUser >> registerR
    , dir "activate"      $ requireNoUser >> activateR
    , dir "lost-password" $ requireNoUser >> lostPasswordR
    , dir "settings"      $ requireUser  >>= settingsR
    , dir "profile"       $ requireUser  >>= profileR
    ]


--------------------------------------------------------------------------------
-- Log in/out

loginR :: NPaste ()
loginR = choice
  [ do methodM POST
       pdata <- getPostData
       choice
         [ do -- require that both email and password are correct
              u    <- require $ getUserByEmail  (getValue pdata "email")
              isOk <-           checkPassword u (getValue pdata "password")
              unless isOk mzero
              performLogin u
              -- forward to index page
              ResponseCode .= seeOther ("/u" :: String)
              HtmlBody     .= loginCorrectHtml
           -- throw error otherwise
         , HtmlBody .= loginHtml (Just "Wrong email or password.") pdata
         ]
  , HtmlBody .= loginHtml Nothing nullPostData
  ]

performLogin :: User -> NPaste ()
performLogin u = do
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
 where
  getHeader' h = fmap B8.unpack . getHeader h

logoutR :: User -> NPaste ()
logoutR _ = choice
  [ do methodM POST
       pdata <- getPostData
       choice
         [ do when (null $ getValue pdata "no-logout") mzero
              ResponseCode .= seeOther ("/u" :: String)
              HtmlBody     .= logoutCancelHtml
         , do when (null $ getValue pdata "logout") mzero
              s <- requireSession
              rmSession s
              expireCookie "sessionId"
              ResponseCode .= seeOther ("/" :: String)
              HtmlBody     .= logoutSuccessfulHtml
         ]
  , do HtmlBody     .= logoutHtml
  ]

lostPasswordR :: NPaste ()
lostPasswordR = return ()


--------------------------------------------------------------------------------
-- Profile & settings

profileR :: User -> NPaste ()
profileR u = do

  modifyNP_ $ \(CSS cur) ->
    CSS $ cur ++ ["code/hk-pyg.css", "code.css", "view.css"]

  f <- choice
    [ do methodM POST
         decodeBody (defaultBodyPolicy "/tmp/" 0 100000 100000)
         body $ look "filter"
    , fmap unwords buildFilterFromUrl
    ]

  case parseFilter f of
       Left err -> do
         Title      .= Just "My pastes"
         HtmlBody   .= profileHtml (Just f) (Left err)
       Right (filterToSearch -> Just s) -> do
         p <- findPastes 20 0 $ S_And (S_User u) s
         Title      .= Just "My pastes (filtered)"
         HtmlBody   .= profileHtml (Just f) (Right p)
       _ -> do
         p <- getRecentPastes (Just u) 20 0 True
         Title      .= Just "My pastes"
         HtmlBody   .= profileHtml Nothing (Right p)

settingsR :: User -> NPaste ()
settingsR _ = return () -- TODO


--------------------------------------------------------------------------------
-- Registration & activation

registerR :: NPaste ()
registerR = choice
  [ do methodM POST
       pdata <- getPostData
       -- add inactive user
       choice
         [ do let email = getValue pdata "email"
                  uname = getValue pdata "username"
                  pw    = getValue pdata "password"
              when (any null [email, uname, pw]) mzero
              -- TODO: validate email?
              mu <- addUser uname pw (Just email)
              case mu of
                   Left err -> HtmlBody .= registerHtml (Just err) pdata
                   Right  u -> do
                     akey <- genActivationKey
                     addInactiveUser u akey
                     -- TODO: send email with activation key
                     HtmlBody .= registerCompleteHtml email
         , HtmlBody .= registerHtml (Just $ AUE_Other "Missing email, username or password.") pdata
         ]
  , HtmlBody .= registerHtml Nothing nullPostData
  ]
 where
  genActivationKey = do
    g <- liftIO newStdGen
    return $ map (chars !!) . take 15 $ randomRs (0,length chars-1) g
  chars = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']

activateR :: NPaste ()
activateR = choice
  [ dir "id" $ path $ \uidStr ->
      dir "key" $ path $ \akey -> do
        uid     <- maybe mzero return $ readMaybe uidStr
        success <- rmInactiveUser uid akey
        if success then do
           Just u <- getUserById uid
           performLogin u
           HtmlBody .= activateSuccessHtml u
         else
           HtmlBody .= activateFailHtml "Incorrect activation key."
  , HtmlBody .= activateFailHtml "Incomplete information."
  ]


--------------------------------------------------------------------------------
-- User management

-- | Get the current session/user from the session cookie. This function should
-- probably be used only once (hence the awfully long name).
initSession :: NPaste ()
initSession = try $ do
  s  <- lookCookieValue "sessionId"
  rq <- askRq
  let ip = fromMaybe (fst $ rqPeer rq) $
            getHeader' "x-forwarded-for" (rqHeaders rq)
      ua = fromMaybe "" $
            getHeader' "user-agent"      (rqHeaders rq)
  ms <- getSession s ip ua
  CurrentSession .= ms
 where
  try f = choice [f, return ()]
  getHeader' h = fmap B8.unpack . getHeader h

getCurrentUser :: NPaste (Maybe User)
getCurrentUser = (join . fmap sessionUser) `fmap` getCurrentSession

requireUser :: NPaste User
requireUser =
  maybe mzero return =<< getCurrentUser

requireNoUser :: NPaste ()
requireNoUser =
  maybe (return ()) (const mzero) =<< getCurrentUser


--------------------------------------------------------------------------------
-- Session management

getCurrentSession :: NPaste (Maybe Session)
getCurrentSession = unCurrentSession `fmap` getNP

requireSession :: NPaste Session
requireSession =
  maybe mzero return =<< getCurrentSession


--------------------------------------------------------------------------------
-- Menu generation

setupUserMenu :: NPaste ()
setupUserMenu = do
  initSession
  mu <- getCurrentUser
  case mu of
       Just u  -> MenuStructure .= userMenu u
       Nothing -> MenuStructure .= anonMenu



--------------------------------------------------------------------------------
-- Post data

getPostData :: NPaste PostData
getPostData = do
  decodeBody (defaultBodyPolicy "/tmp/npaste.de/" 1000000 1000000 1000000)
  fmap PostData $ body lookPairs

nullPostData :: PostData
nullPostData = PostData []
