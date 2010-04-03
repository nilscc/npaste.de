module Paste.View
    ( htmlOpts
    , htmlBody
    , htmlBody'
    , getLogin
    , module App.View
    ) where

import HSP
import Happstack.Server
import Happstack.State
import qualified Happstack.Auth as Auth

import App.View
import Paste.View.Menu
import Util.Control
import Paste.Types

-- | Default html options
htmlOpts :: [HtmlOptions]
htmlOpts = [ WithCss    (CssFile "/static/style.css")
           , WithTitle  "npaste.de"
           , WithLogo   "npaste.de" "IO String" "a haskell happstack pastebin"
           ]

-- | Default HTML body
htmlBody :: [HSP XML] -> ServerPart Response
htmlBody elem = do
    login <- getLogin
    htmlBody' login elem

htmlBody' :: LoggedIn -> [HSP XML] -> ServerPart Response
htmlBody' login elem = do
    uname <- case login of

                  LoggedInAs skey -> do
                      s <- query $ Auth.GetSession skey
                      case s of

                           Just Auth.SessionData { Auth.sesUsername = Auth.Username un } ->
                               return $ Just un
                           _ -> return Nothing

                  _ -> return Nothing

    xmlResponse . HtmlBody htmlOpts $ [menuHsp uname login] ++ elem
