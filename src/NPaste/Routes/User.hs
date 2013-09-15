{-# LANGUAGE OverloadedStrings, ViewPatterns, NamedFieldPuns #-}

module NPaste.Routes.User
  ( userR
  ) where

import qualified Data.ByteString.Char8 as B8
import Data.Either
import Data.Maybe
import Data.List (intercalate)
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
    , dir "profile"       $                  profileR
    , dir "settings"      $ requireLogin "Settings" settingsR
    , dir "logout"        $ requireUser  >>= logoutR
    , dir "login"         $ requireNoUser >> loginR
    , dir "register"      $ requireNoUser >> registerR
    , dir "activate"      $ requireNoUser >> activateR
    , dir "lost-password" $ requireNoUser >> lostPasswordR
    ]

invalidPw :: String -> Bool
invalidPw pw = length pw < 6

--------------------------------------------------------------------------------
-- Log in/out

loginR :: NPaste ()
loginR = choice
  [ do methodM POST
       pdata <- getPostData
       choice
         [ do -- require that both email and password are correct
              u    <- require $ getUserByEmail  (B8.unpack $ getValue pdata "email")
              isOk <-           checkPassword u (B8.unpack $ getValue pdata "password")
              unless isOk mzero
              performLogin u
              -- forward to index page
              ResponseCode .= seeOther ("/" :: String)
              HtmlBody     .= loginCorrectHtml
         , -- throw error otherwise
           HtmlBody .= loginHtml (Just "Wrong email or password.") pdata
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
  -- add session
  s  <- addSession (Just u) ip ua
  CurrentSession .= Just s
  -- set session cookie
  let clife = Expires (sessionExpires s)
      ck    = mkCookie "sessionId" (sessionId s)
  addCookie clife ck
  -- update menu structure
  am <- getsNP unActiveMenu
  case am of
       Just (M_User Nothing) -> ActiveMenu .= Just $ M_User (Just u)
       _                     -> return ()
  MenuStructure .= userMenu u
 where
  getHeader' h = fmap B8.unpack . getHeader h

-- perform login if necessary
requireLogin :: String      -- ^ <H1> content
             -> NPaste ()
             -> NPaste ()
requireLogin h1 doWhenLoggedIn = choice
  [ requireUser >>= \_ -> doWhenLoggedIn
  , do method POST
       pdata <- getPostData
       u     <- require $ getUserByEmail  (B8.unpack $ getValue pdata "email")
       isOk  <-           checkPassword u (B8.unpack $ getValue pdata "password")
       unless isOk mzero
       performLogin u
       localRq (\rq -> rq{ rqMethod = GET }) doWhenLoggedIn
  , do -- show login form if we're not logged in
       pdata <- getPostData
       rq    <- askRq
       err   <- choice [ method POST >> return "Wrong email or password."
                       ,                return "Login required" ]
       HtmlBody .= requireLoginHtml h1 (rqUri rq ++ rqQuery rq) (Just err) pdata
  ]

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
lostPasswordR = do

  pdata <- getPostData

  choice
    [ dir "change" $ do
        res <- choice
          [ do methodM POST

               let email = B8.unpack $ getValue pdata "email"
                   key   = B8.unpack $ getValue pdata "key"
                   pw1   = B8.unpack $ getValue pdata "pw1"
                   pw2   = B8.unpack $ getValue pdata "pw2"

               mu <- getUserByEmail email
               case mu of
                    Nothing                -> err "Unknown email address."
                    Just u | pw1 /= pw2    -> err "Both passwords have to match."
                           | invalidPw pw1 -> err "Invalid password. Make sure it is at least 6 characters long."
                           | otherwise     -> do
                      s <- changeLostPassword u key pw1
                      if not s then err "Invalid key." else do
                         performLogin u
                         success (u,"Password changed.")
          , return Nothing
          ]
        case res of
             Just (Right (u,s)) -> HtmlBody .= settingsHtml u (Just s) []
             _                  -> HtmlBody .= lostPasswordChangeHtml res pdata
    , do res <- choice
           [ do methodM POST
                let email = B8.unpack $ getValue pdata "email"
                mu <- getUserByEmail email
                case mu of
                     Nothing -> err "Unknown email address."
                     Just u  -> do
                       k <- genActivationKey
                       addLostPasswordKey u k
                       sendEMail [(Nothing, email)]
                                 "npaste.de - Your lost password" $
                                 "Hello " ++ userName u ++ ",\n\n\
                                 \You have requested a new password. To set your \
                                 \new password, go to:\n\n\
                                 \  https://npaste.de/u/lost-password/change\n\n\
                                 \And enter the following key:\n\n\
                                 \  " ++ k ++ "\n\n\
                                 \If you did not request a new password, please \
                                 \ignore this email and do not reply to it.\n\n\n\
                                 \Thank you for using npaste.de,\n\n\
                                 \ - your webmaster"
                       success $ "An email has been sent to \"" ++ email ++ "\"."
           , return Nothing
           ]
         HtmlBody .= lostPasswordHtml res pdata
    ]
 where
  err     = return . Just . Left
  success = return . Just . Right


--------------------------------------------------------------------------------
-- Profile & settings

profileR :: NPaste ()
profileR = do -- | TODO: Show statistics instead of current profile and use @user show a users pastes

  modifyNP_ $ \(CSS cur) ->
    CSS $ cur ++ ["code.css", "view.css"]

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
             Title .= Just $ "Pastes by " ++ userName
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
             Title    .= Just "User not found"
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


settingsR :: NPaste ()
settingsR = do
  Title .= Just "Settings"
  choice
    [ -- verify/activate new email addresses
      dir "new-email" $ path $ \akey -> do
        s <- requireSession
        u <- requireUser
        r <- activateEmail u akey
        case r of
             Just newEmail -> do
               let u' = u{ userEmail = Just newEmail }
               CurrentSession .= Just s{ sessionUser = Just u' }
               HtmlBody       .= settingsHtml u' (Just "Email verified.") []
             Nothing -> do
               HtmlBody       .= settingsHtml u Nothing [ "Invalid email verification code." ]

    , do -- change and save new settings
         methodM POST
         u <- requireUser

         -- account details
         pdata <- getPostData
         let email = B8.unpack $ getValue pdata "email" 
             pw1   = B8.unpack $ getValue pdata "pw1"
             pw2   = B8.unpack $ getValue pdata "pw2"
             pwCur = B8.unpack $ getValue pdata "pw-cur"

         -- change email
         emailRes <- runErrorT $
           if (null email || userEmail u == Just email) then return Nothing else do
              when (isLeft $ validate $ B8.pack email) $
                 throwError "Invalid email address."
              akey <- genActivationKey
              suc  <- addIncativeEmail u email akey
              -- send activation email on success
              if suc then do
                 sendEMail [(Nothing, email)]
                           "npaste.de - Email address verification" $
                           "Hello " ++ userName u ++ ",\n\n\
                           \You have requested to change your email. To verify your \
                           \new email address, go to:\n\n\
                           \  https://npaste.de/u/settings/new-email/" ++ akey ++ "\n\n\
                           \Please do not reply to this email.\n\n\n\
                           \Thank you for using npaste.de,\n\n\
                           \ - your webmaster"
                 return . Just $
                   "The verification code for your new email address has been sent to \""
                   ++ email ++ "\"."
               else
                 throwError "Email address already in use."

         -- change password
         pwRes <- runErrorT $
           if (null pw1 && null pw2) then return Nothing else do
              when (pw1 /= pw2) $
                throwError "New password and password confirmation have to match."
              pwOk <- checkPassword u pwCur
              unless pwOk $
                throwError "Current password is wrong."
              updateUserPassword u pw1
              return $ Just "Password changed."

         -- update user settings
         let u' = u { userDefaultHidden = getValue pdata "default_hidden" == "on"
                    , userPublicProfile = getValue pdata "public_profile" == "on" }
         updateUserSettings u'

         let (errs,msgs) = partitionEithers [emailRes, pwRes]
             msgs' | null msgs = []
                   | otherwise = "Settings saved." : catMaybes msgs
         HtmlBody .= settingsHtml u' (Just $ intercalate " " msgs') errs

    , do -- show default page
         u <- requireUser
         HtmlBody .= settingsHtml u Nothing []
    ]
 where
  isLeft (Left  _) = True
  isLeft (Right _) = False
  

--------------------------------------------------------------------------------
-- Registration & activation

registerR :: NPaste ()
registerR = choice
  [ do methodM POST
       pdata <- getPostData
       -- add inactive user
       choice
         [ do let email = B8.unpack $ getValue pdata "email"
                  uname = B8.unpack $ getValue pdata "username"
                  pw    = B8.unpack $ getValue pdata "password"
              when (any null [email, uname] || invalidPw pw) mzero
              case validate $ B8.pack email of
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
                                     "npaste.de - Your activation key" $
                                     "Hello " ++ uname ++ ",\n\n\
                                     \Your registration on npaste.de was successful. To activate your account, \
                                     \go to:\n\n\
                                     \  https://npaste.de/u/activate/id/" ++ show( userId u) ++ "/key/" ++ akey ++ "\n\n\
                                     \If you did not request this account, please ignore this email and do not \
                                     \reply to it.\n\n\n\
                                     \Thank you for using npaste.de,\n\n\
                                     \ - your webmaster"
                           HtmlBody .= registerCompleteHtml email
         , HtmlBody .= registerHtml (Just $ AUE_Other "Missing email, username or password \
                                                      \(make sure it is at least 6 characters long).")
                                    pdata
         ]
  , HtmlBody .= registerHtml Nothing nullPostData
  ]

genActivationKey :: MonadIO m => m String
genActivationKey = do
  g <- liftIO newStdGen
  return $ map (chars !!) . take 15 $ randomRs (0,length chars-1) g
 where
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
