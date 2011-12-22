{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

module NPaste.Html.User where

import Text.Blaze ((!), toHtml)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.ParserCombinators.Parsec.Error

import NPaste.Types
import NPaste.Html.View
import NPaste.Html.Read

--------------------------------------------------------------------------------
-- Log in/out

loginHtml :: Maybe String -> PostData -> Html
loginHtml err pdata = do

  H.h1 "User login"

  maybe (return ()) ((H.p ! A.class_ "error") . toHtml) err

  H.form ! A.action "/u/login" ! A.method "post" ! A.id "login-form" $ do
    H.p "Email:"
    H.input ! A.type_ "text" ! A.name "email" ! A.value (pdata ? "email")
    H.br
    H.p "Password:"
    H.input ! A.type_ "password" ! A.name "password"
    H.br
    H.input ! A.type_ "submit" ! A.value "Login"

  H.p $ do
    "No user account yet? "
    H.a ! A.href "/u/register" $ "Register"
    " now for free. Forgot your password? "
    H.a ! A.href "/u/lost-password" $ "Get a new one."

loginCorrectHtml :: Html
loginCorrectHtml = do

  H.h1 "User login"

  H.p ! A.class_ "success" $ do
    "Login correct. Click "
    H.a ! A.href "/" $ "here"
    " to get back to the start page."

logoutHtml :: Html
logoutHtml = do

  H.h1 "Log out"

  H.p "Are you sure you want to log out?"

  H.p ! A.id "logout-links" $ do
    H.a ! A.href "/u/logout/confirm" $ "Yes, log me out!"
    H.a ! A.href "/u"                $ "No, get me back to my profile."

logoutSuccessfulHtml :: Html
logoutSuccessfulHtml = do

  H.h1 "Log out"

  H.p ! A.class_ "success" $ "Logout successful"

logoutCancelHtml :: Html
logoutCancelHtml = do

  H.h1 "Log out"

  H.p $ do
    "Logout canceled. You should be forwarded to your "
    H.a ! A.href "/u" $ "profile"
    "."

lostPasswordHtml :: PostData -> Html
lostPasswordHtml _ = return ()


--------------------------------------------------------------------------------
-- Registration & activation

registerHtml :: Maybe AddUserError -> PostData -> Html
registerHtml merr pdata = do

  H.h1 "Register a new user"

  H.p "Please make sure your email address is correct. It will be used to\
      \ send you the activation key required to activate your account."

  maybe (return ()) aueToHtml merr

  H.form ! A.method "post" ! A.action "/u/register" ! A.id "registration-form" $ do
    H.p "Email:"
    H.input ! A.type_ "text"     ! A.name "email"    ! A.value (pdata ? "email")
    H.br
    H.p "Username:"
    H.input ! A.type_ "text"     ! A.name "username" ! A.value (pdata ? "username")
    H.br
    H.p "Password:"
    H.input ! A.type_ "password" ! A.name "password"
    H.br
    H.input ! A.type_ "submit"

aueToHtml :: AddUserError -> Html
aueToHtml (AUE_InvalidUsername uname) =
  H.p ! A.class_ "error" $ toHtml $ "The username \"" ++ uname ++ "\" is invalid."
aueToHtml AUE_NoPassword =
  H.p ! A.class_ "error" $ "Missing password."
aueToHtml AUE_AlreadyExists =
  H.p ! A.class_ "error" $ "User already exists."
aueToHtml (AUE_Other s) =
  H.p ! A.class_ "error" $ toHtml s

registerCompleteHtml :: String  -- ^ email address
                     -> Html
registerCompleteHtml email = do

  H.h1 "Register a new user"

  H.p ! A.class_ "success" $ do
    "Registration complete. An email with your activation key has been send to \""
    toHtml email
    "\"."

activateSuccessHtml :: User -> Html
activateSuccessHtml User{ userName } = do

  H.h1 "User activation"

  H.p ! A.class_ "success" $ do
    "User \""
    toHtml userName
    "\" successfully activated."

  H.p $ do
    H.a ! A.href "/u" $ "View your profile"
    H.a ! A.href "/"  $ "Add new pastes"

activateFailHtml :: String -> Html
activateFailHtml err = do

  H.h1 "User activation"

  H.p ! A.class_ "error" $ toHtml err


--------------------------------------------------------------------------------
-- Profile & settings

profileHtml :: Maybe String               -- ^ user name (if /u/profile/<name>)
            -> Maybe String               -- ^ filter string
            -> Either ParseError [Paste]
            -> Html
profileHtml muname mf epastes = do
  H.h1 $
    case muname of
         Nothing -> do
           "My pastes"
           maybe (return ()) (const " (filtered)") mf
         Just uname -> do
           "Pastes by "
           toHtml uname
           maybe (return ()) (const " (filtered)") mf
  filterForm "/u" mf
  H.div ! A.id "paste_list" $
    case epastes of
         Right [] -> H.p ! A.class_ "error" $ "No pastes found."
         Left err -> H.p ! A.class_ "error" $ toHtml $
           showErrorMessages "or" "?" "instead of"
             "Invalid filter request: Unexpected" "end of input"
             (errorMessages err)
         Right ps -> listPastes ps (Just 20)

profileErrorHtml :: Maybe User -> String -> Html
profileErrorHtml mu err = do

  H.h1 $ case mu of
              Just User{ userName } -> do
                "Pastes by "
                toHtml userName
              _ -> "Error"

  H.p ! A.class_ "error" $ toHtml err

settingsHtml :: PostData -> Html
settingsHtml _ = return ()
