{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Paste.View.Register
    ( showRegister
    ) where

import Control.Monad                (msum)
import Control.Monad.Error          (runErrorT)
import Data.Maybe                   (fromMaybe, isJust)
import HSP
import Happstack.Server

import App.View                     (xmlResponse, HtmlBody (..))
import Paste.Post.Register          (register)
import Paste.View                   (getLogin, htmlBody)
import Paste.View.Menu              (menuHsp)
import Paste.Types                  (LoggedIn (..))

showRegister :: ServerPart Response
showRegister = do
    login <- getLogin
    nick    <- getDataBodyFn $ look "nick"
    email   <- getDataBodyFn $ look "email"
    msum
        [ do
            methodM POST
            reg <- runErrorT $ register
            case reg of
                 Left  e -> xmlResponse $ htmlBody login [registerHsp (Just $ show e) nick email]
                 Right n -> xmlResponse $ htmlBody login [successHsp n]
        , do
            case login of
                 NotLoggedIn -> xmlResponse $ htmlBody login [registerHsp Nothing nick email]
                 _           -> xmlResponse $ htmlBody login [alreadyLoggedInHsp]
        ]

type Nick  = String
type Email = String

alreadyLoggedInHsp :: HSP XML
alreadyLoggedInHsp =
    <div id="main">
        <p>Already logged in!</p>
    </div>

-- | Registration form HSP
registerHsp :: Maybe String     -- ^ error message
            -> Maybe Nick
            -> Maybe Email
            -> HSP XML
registerHsp err nick email =
    <div id="main">
        <h1>Register</h1>
        <p>To get a new account, please enter your desired nick (only chars from A-Za-z and numbers from 0-9) and your email adress below:</p>
        <form id="register-form" action="/?view=register" method="post">
            <%
                if isJust err
                   then [<p class="error">Error: <% err %></p>]
                   else []
            %>
            <table>
                <tr>
                    <td>Name:</td>
                    <td><input type="text" name="nick" value=(fromMaybe "" nick) /></td>
                </tr>
                <tr>
                    <td>Email:</td>
                    <td><input type="text" name="email" value=(fromMaybe "" email) /></td>
                </tr>
            </table>
            <p><input type="submit" /></p>
        </form>
        <p>You will get an email as soon as possible with your activation link.</p>
    </div>

successHsp :: Nick -> HSP XML
successHsp nick =
    <div id="main">
        <h1>Welcome <% nick %></h1>
        <p>Your registration was successfull. You should receive your activation key as soon as possible.</p>
    </div>
