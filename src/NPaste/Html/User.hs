{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

module NPaste.Html.User where

import Text.Blaze ((!), toHtml)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import NPaste.Types

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

  H.p $ do
    "Login correct. Click "
    H.a ! A.href "/" $ "here"
    " to get back to the start page."

registerHtml :: PostData -> Html
registerHtml _ = return ()

settingsHtml :: PostData -> Html
settingsHtml _ = return ()

profileHtml :: PostData -> Html
profileHtml _ = return ()

lostPasswordHtml :: PostData -> Html
lostPasswordHtml _ = return ()
