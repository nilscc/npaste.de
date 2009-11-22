{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Paste.View.Login
    ( registerBody
    , loginBody
    , showRegister
    , showLogin
    ) where

import Happstack.Server
import HSP
import App.View
import Paste.View.Menu
import Paste.View.Logo


-- | show login page
showLogin :: ServerPartT IO Response
showLogin = xmlResponse $ htmlBody [loginBody]

-- | show register page
showRegister :: ServerPartT IO Response
showRegister = xmlResponse $ htmlBody [registerBody]


htmlBody = HtmlBody htmlOpts . (++) [ logoHsp "npaste.de" "IO String" "a pastebin running on happstack"
                                    , menuHsp NotLoggedIn
                                    ]

htmlOpts :: [HtmlOptions]
htmlOpts = [ WithTitle  $ "npaste.de - Register"
           , WithCss    $ CssFile "/static/style.css"
           ]


registerBody =
    <div id="main">
        <h1>Register</h1>
        <p>You're new to npaste.de and want your own user name? Just enter your desired username and password below.</p>
        <form id="register" action="http://npaste.de/register" method="POST">
            <table>
                <tr>
                    <td><p>Username:</p></td>
                    <td><input type="text" name="username" class="text" /></td>
                </tr>
                <tr>
                    <td><p>Password:</p></td>
                    <td><input type="password" name="password" class="text" /></td>
                </tr>
            </table>

            <p>Please select your default settings (or just hit the submit button to use default settings):</p>
            <p>
                <input type="radio" name="paste-public" value="everybody" checked="checked" /> Show my pastes to everybody (default).<br />
                <input type="radio" name="paste-public" value="known" /> Show my pastes only to people I know.<br />
                <input type="radio" name="paste-public" value="hide" /> Hide all my pastes.<br />
            </p>

            <input type="submit" name="submit" />
            <input type="reset" />
        </form>
    </div>



loginBody =
    <div id="register">
        <h1>Login</h1>
        <p><% "Please login:" %></p>
        <form id="register" action="http://npaste.de/login" method="POST">
            <table>
                <tr>
                    <td><p>Username:</p></td>
                    <td><input type="text" name="username" class="text" /></td>
                </tr>
                <tr>
                    <td><p>Password:</p></td>
                    <td><input type="password" name="password" class="text" /></td>
                </tr>
            </table>

            <input type="submit" name="submit" value="Login" />
        </form>
        <p>You can also <a href="http://npaste.de/lost-my-password">retrieve your password</a> or
        <a href="http://npaste.de/register">register</a>.</p>
    </div>
