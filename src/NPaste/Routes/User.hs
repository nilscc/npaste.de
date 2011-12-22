{-# LANGUAGE OverloadedStrings, ViewPatterns, NamedFieldPuns #-}

module NPaste.Routes.User
  ( userR
  ) where

import qualified Data.ByteString.Char8 as B8
import Data.Maybe
import Happstack.Server hiding (require, addCookie, expireCookie)
import System.Random
import Text.Email.Validate

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
                          ,                  profileR ]
    , dir "login"         $ requireNoUser >> loginR
    , dir "logout"        $ requireUser  >>= logoutR
    , dir "register"      $ requireNoUser >> registerR
    , dir "activate"      $ requireNoUser >> activateR
    , dir "lost-password" $ requireNoUser >> lostPasswordR
    , dir "settings"      $ requireUser  >>= settingsR
    , dir "profile"       $                  profileR
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
  -- update menu
  MenuStructure .= userMenu u
 where
  getHeader' h = fmap B8.unpack . getHeader h

logoutR :: User -> NPaste ()
logoutR _ = choice
  [ do nullDir
       HtmlBody     .= logoutHtml
  , dir "confirm" $ do
       rmSession =<< requireSession
       expireCookie "sessionId"
       ResponseCode .= seeOther ("/" :: String)
       HtmlBody     .= logoutSuccessfulHtml
  ]

lostPasswordR :: NPaste ()
lostPasswordR = return ()


--------------------------------------------------------------------------------
-- Profile & settings

profileR :: NPaste ()
profileR = do

  modifyNP_ $ \(CSS cur) ->
    CSS $ cur ++ ["code/hk-pyg.css", "code.css", "view.css"]

  f <- choice
         [ do methodM POST
              decodeBody (defaultBodyPolicy "/tmp/" 0 100000 100000)
              body $ look "filter"
         , fmap unwords buildFilterFromUrl
         ]

  choice

      -- /u or /u/profile
    [ do nullDir
         u <- requireUser
         myProfile u f

      -- /u/profile/<username>
    , path $ \uname -> choice

        [ do -- <username> == current user -> show my own profile
             u <- requireUser
             unless (userName u == uname) mzero
             myProfile u f

        , do -- <username> has deactivated his public profile
             u <- maybe mzero return =<< getUserByName uname
             when (userPublicProfile u) mzero
             ActiveMenu .= Just $ M_View Nothing
             HtmlBody   .= profileErrorHtml (Just u) "User has deactivated his public profile."

        , do -- <username> is an existing user
             u@User{ userName } <- maybe mzero return =<< getUserByName uname
             Title .= Just $ "Profile: " ++ userName
             case parseFilter f of
                  Left err -> do
                    ActiveMenu .= Just $ M_View Nothing
                    HtmlBody   .= profileHtml (Just userName) (Just f) (Left err)
                  Right fl@(filterToSearch -> Just s) -> do
                    p <- findPastes 20 0 $ S_And (S_User u) s
                    ActiveMenu .= Just $ M_View (Just fl)
                    HtmlBody   .= profileHtml (Just userName) (Just f) (Right p)
                  _ -> do
                    p <- getRecentPastes (Just u) 20 0 False
                    ActiveMenu .= Just $ M_View Nothing
                    HtmlBody   .= profileHtml (Just userName) Nothing (Right p)

        , do -- <username> is no valid user
             Title    .= Just "Profile: User not found"
             HtmlBody .= profileErrorHtml Nothing "User not found"
        ]
    ]
 where
  myProfile u f =
    case parseFilter f of
         Left err -> do
           Title      .= Just "My pastes"
           HtmlBody   .= profileHtml Nothing (Just f) (Left err)
         Right (filterToSearch -> Just s) -> do
           p <- findPastes 20 0 $ S_And (S_User u) s
           Title      .= Just "My pastes (filtered)"
           HtmlBody   .= profileHtml Nothing (Just f) (Right p)
         _ -> do
           p <- getRecentPastes (Just u) 20 0 True
           Title      .= Just "My pastes"
           HtmlBody   .= profileHtml Nothing Nothing (Right p)


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
              case validate email of
                   Left  _ -> HtmlBody .= registerHtml (Just $ AUE_Other "Invalid email address.")
                                                       pdata
                   Right _ -> do
                     mu  <- addUser uname pw (Just email)
                     case mu of
                         Left err -> HtmlBody .= registerHtml (Just err) pdata
                         Right  u -> do
                           akey <- genActivationKey
                           addInactiveUser u akey
                           sendEMail [(Nothing, email)]
                                     "Your npaste.de activation key" $
                                     "Hello " ++ uname ++ "!\n\n\
                                     \Your registration on npaste.de was successful. To activate your account, \
                                     \go to:\n\n\
                                     \  http://npaste.de/u/activate/id/" ++ show( userId u) ++ "/key/" ++ akey ++ "\n\n\
                                     \If you did not request this account, please ignore this email and do not \
                                     \reply to it.\n\n\n\
                                     \Thank you for using npaste.de,\n\n\
                                     \ - your webmaster"
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
           ActiveMenu .= Just $ M_User (Just u)
           HtmlBody   .= activateSuccessHtml u
         else
           HtmlBody .= activateFailHtml "Incorrect activation key."
  , HtmlBody .= activateFailHtml "Incomplete information."
  ]


--------------------------------------------------------------------------------
-- Post data

getPostData :: NPaste PostData
getPostData = do
  decodeBody (defaultBodyPolicy "/tmp/npaste.de/" 1000000 1000000 1000000)
  fmap PostData $ body lookPairs

nullPostData :: PostData
nullPostData = PostData []
