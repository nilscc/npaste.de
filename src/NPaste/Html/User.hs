{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

module NPaste.Html.User where

import Data.Maybe
import Text.Blaze ((!), toHtml, toValue)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.ParserCombinators.Parsec.Error

import NPaste.Types
import NPaste.Utils
import NPaste.Html.View
import NPaste.Html.Read

--------------------------------------------------------------------------------
-- Log in/out

loginHtml :: Maybe String -> PostData -> Html
loginHtml err pdata =
  requireLoginHtml "User login" "/u/login" err pdata

loginForm :: AttributeValue   -- ^ action url
          -> PostData
          -> Html
loginForm action pdata = do

  H.form ! A.action action ! A.method "post" ! A.id "login-form" $ do
    H.p ! A.class_ "label" $ "Email:"
    H.input ! A.type_ "text" ! A.name "email" ! A.value (pdata ? "email")
    H.br
    H.p ! A.class_ "label" $ "Password:"
    H.input ! A.type_ "password" ! A.name "password"
    H.br
    H.input ! A.type_ "submit" ! A.value "Login"

loginCorrectHtml :: Html
loginCorrectHtml = do

  H.h1 "User login"

  H.p ! A.class_ "success" $ do
    "Login correct. Click "
    H.a ! A.href "/" $ "here"
    " to get back to the start page."

requireLoginHtml :: String           -- ^ <h1> content
                 -> String           -- ^ action link for login form
                 -> Maybe String     -- ^ error message
                 -> PostData
                 -> Html
requireLoginHtml h1 action merr pdata = do

  H.h1 $ toHtml h1
  withJust_ merr $ (H.p ! A.class_ "error") . toHtml
  loginForm (toValue action) pdata

  H.p $ do
    "No user account yet? "
    H.a ! A.href "/u/register" $ "Register"
    " now for free. Forgot your password? "
    H.a ! A.href "/u/lost-password" $ "Get a new one."

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

lostPasswordHtml :: Maybe (Either String String) -> PostData -> Html
lostPasswordHtml res pdata = do

  H.h1 "Lost password"

  H.p "To request a new password, please enter your email below. You will then \
      \receive an email with further instructions."

  H.p $ do
    "If you already received your key you can change your password "
    H.a ! A.href "/u/lost-password/change" $ "here"
    "."

  withJust_ res $
    either ((H.p ! A.class_ "error")   . toHtml)
           ((H.p ! A.class_ "success") . toHtml)

  unless (isSuccess res) $ do
    H.form ! A.method "post" ! A.action "/u/lost-password"
           ! A.id "lost-password-form" $ do
      H.p ! A.class_ "label" $ "Email:"
      H.input ! A.type_ "text" ! A.name "email" ! A.value (pdata? "email")
      H.input ! A.type_ "submit"
 where
  isSuccess (Just (Right _)) = True
  isSuccess _                = False

lostPasswordChangeHtml :: Maybe (Either String (User,String)) -> PostData -> Html
lostPasswordChangeHtml res pdata = do

  H.h1 "Lost password"

  H.p "To change your password insert your email and the private key in the \
      \text boxes below."

  withJust_ res $
    either ((H.p ! A.class_ "error")   . toHtml)
           ((H.p ! A.class_ "success") . toHtml . snd)

  H.form ! A.method "post" ! A.action "/u/lost-password/change"
         ! A.id "lost-password-change-form" $ do

    H.p ! A.class_ "label" $ "Your email:"
    H.input ! A.type_ "text" ! A.name "email" ! A.value (pdata ? "email")
    H.br

    H.p ! A.class_ "label" $ "Key:"
    H.input ! A.type_ "text" ! A.name "key" ! A.value (pdata ? "key")
    H.br

    H.p ! A.class_ "label" $ "New password:"
    H.input ! A.type_ "password" ! A.name "pw1"
    H.br

    H.p ! A.class_ "label" $ "Confirm password:"
    H.input ! A.type_ "password" ! A.name "pw2"

    H.input ! A.type_ "submit"

--------------------------------------------------------------------------------
-- Registration & activation

registerHtml :: Maybe AddUserError -> PostData -> Html
registerHtml merr pdata = do

  H.h1 "Register a new user"

  H.p "Please make sure your email address is correct. It will be used to\
      \ send you the activation key required to activate your account."

  withJust_ merr aueToHtml

  H.form ! A.method "post" ! A.action "/u/register" ! A.id "registration-form" $ do
    H.p ! A.class_ "label" $ "Email:"
    H.input ! A.type_ "text"     ! A.name "email"    ! A.value (pdata ? "email")
    H.br
    H.p ! A.class_ "label" $ "Username:"
    H.input ! A.type_ "text"     ! A.name "username" ! A.value (pdata ? "username")
    H.br
    H.p ! A.class_ "label" $ "Password:"
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

  H.p ! A.id "user-activated-links" $ do
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
           withJust_ mf $ const " (filtered)"
         Just uname -> do
           "Pastes by "
           toHtml uname
           withJust_ mf $ const " (filtered)"
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

settingsHtml :: User
             -> (Maybe String)
             -> [String]
             -> Html
settingsHtml u success errs = do

  H.h1 "Settings"

  withJust_ success $ \s ->
    H.p ! A.class_ "success" $ toHtml s

  unless (null errs) $ do
    H.p ! A.class_ "error" $
      "There have been errors. Not all of your changes might have been saved:"
    H.ul $ forM_ errs $ \err ->
      H.li $ H.p ! A.class_ "error" $ toHtml err

  H.form ! A.method "post" ! A.action "/u/settings" ! A.id "settings-form" $ do

    H.h3 "Account details"

    H.p ! A.class_ "label" $ "Email:" 
    H.input ! A.type_ "text" ! A.name "email" !
      A.value (toValue . fromMaybe "" $ userEmail u)
    H.br

    H.p ! A.class_ "label" $ "New password:" 
    H.input ! A.type_ "password" ! A.name "pw1"
    H.br

    H.p ! A.class_ "label" $ "Confirm password:" 
    H.input ! A.type_ "password" ! A.name "pw2"
    H.br

    H.p ! A.class_ "label" $ "Current password:"
    H.input ! A.type_ "password" ! A.name "pw-cur"
    H.br

    H.h3 "Paste settings"

    (if userDefaultHidden u then (! A.checked "checked") else id)
      H.input ! A.type_ "checkbox" ! A.name "default_hidden" -- TODO: checked
    H.p ! A.class_ "checkbox" $ "Hide all my pastes by default"
    H.br

    (if userPublicProfile u then (! A.checked "checked") else id)
      H.input ! A.type_ "checkbox" ! A.name "public_profile" -- TODO: checked
    H.p ! A.class_ "checkbox" $ "Let other people view my profile"

    H.div ! A.id "buttons" $ do
      H.input ! A.type_ "submit" ! A.value "Save settings"
      H.input ! A.type_ "reset"
