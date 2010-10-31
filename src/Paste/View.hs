{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Paste.View
    ( htmlOpts
    , htmlBody
    , htmlBody'
    , getLogin
    , module App.View
    ) where

import Data.Char (toLower)
import Data.Maybe (isNothing)
import HSP
import Happstack.Server
import Happstack.State
import qualified Happstack.Auth      as Auth

import App.View
import Paste.View.Menu
import Paste.State
import Paste.Recent
import Paste.Types
import Util.Control


-- | Default html options
htmlOpts :: Maybe PasteEntry    -- ^ Latest news for the title
         -> [HtmlOptions]
htmlOpts p =

    [ WithCss    (CssFile "/static/style.css")
    , WithTitle  "npaste.de"
    , WithLogo   "npaste.de" "IO String" title
    ]

  where title = case p of
                     Just PasteEntry { description = PDescription (Just d) } ->
                         <span>whats new? <a href="/?view=news"><% map toLower d %></a></span>
                     _ ->
                         <span>a haskell happstack pastebin</span>

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

    news <- dropWhile (isNothing . unPDescription . description) `fmap` getRecentPastes' (Just "news") 20 True

    xmlResponse . HtmlBody (htmlOpts $ if null news then Nothing else Just (head news)) $
        [menuHsp uname login] ++ elem
