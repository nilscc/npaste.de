{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Paste.View.Recent
    ( showRecent
    ) where

import HSP
import Control.Monad.Trans      (liftIO)
import System.Time              (ClockTime (..), toUTCTime, calendarTimeToString)


import Happstack.Server
import Happstack.State          (query)

import App.View
import Users.State
import Paste.View               (htmlOpts, getLogin)
import Paste.View.Menu          (menuHsp)
import Paste.Types              (LoggedIn (..))
import Paste.State              (GetPastesByUser (..), GetAllEntries (..), PasteEntry (..), Content (..), ID (..))


showRecent = do
    loggedInAs  <- getLogin
    pastes      <- query $ GetAllEntries
    recent      <- mapM makeRecent . take 5 . filter (not . hide) $ pastes
    xmlResponse $ HtmlBody htmlOpts [menuHsp loggedInAs, recentHsp recent Nothing]


--------------------------------------------------------------------------------
-- Data definitions + Helper stuff
--------------------------------------------------------------------------------

data RecentPaste = RecentPaste { rDate      :: ClockTime
                               , rCont      :: String
                               , rDesc      :: Maybe String
                               , rId        :: ID
                               }

makeRecent pe = liftIO $ do
    content <- case content pe of
                    Plain str -> return str
                    File fp   -> readFile fp
    return $ RecentPaste { rDate = date pe
                         , rCont = content
                         , rDesc = description pe
                         , rId   = pId pe
                         }


--------------------------------------------------------------------------------
-- HSP
--------------------------------------------------------------------------------

recentHsp :: [RecentPaste] -> Maybe String -> HSP XML
recentHsp entries user =
    <div id="main">
        <h1>Recent pastes<% maybe "" (" of " ++) user %>:</h1>
        <% entries %>
    </div>


--------------------------------------------------------------------------------
-- XML instances
--------------------------------------------------------------------------------

instance (XMLGenerator m, EmbedAsChild m XML) => (EmbedAsChild m RecentPaste) where
    asChild pe =
        <%
            <div class="recentpaste">
                <p class="paste-info"><a href=("/" ++ id ++ "/")><% "#" ++ id %></a> - <%
                        case rDesc pe of
                             Just d  -> "Description: " ++ d
                             Nothing -> "No description."
                %></p>
                <p class="paste-date">Pasted at: <% calendarTimeToString . toUTCTime $ rDate pe %></p>
                <pre><% ("\n" ++) . unlines . take 8 . lines $ rCont pe %></pre>
            </div>
        %>
      where id = unId $ rId pe
