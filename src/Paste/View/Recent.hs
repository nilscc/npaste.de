{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Paste.View.Recent
    ( showRecent
    ) where


import Data.Maybe (fromMaybe)
import Happstack.Server
import HSP
import System.Time
import System.Locale

import Paste.View
import Paste.View.Pastes
import Paste.Recent
import Paste.State

showRecent :: ServerPart Response
showRecent = do

    recent <- getRecentPastes Nothing 8 True (Just 10)

    htmlBody [recentHsp recent Nothing]


--------------------------------------------------------------------------------
-- HSP
--------------------------------------------------------------------------------

recentHsp :: [RecentPaste] -> Maybe String -> HSP XML
recentHsp entries user =
    <div id="main">
        <h1>Recent pastes<% maybe "" (": " ++) user %></h1>
        <%
            if null entries
               then <% <p>Nothing pasted yet. Be the <a href="/">first</a>!</p> %>
               else <% entries %>
        %>
    </div>


--------------------------------------------------------------------------------
-- XML instances
--------------------------------------------------------------------------------

instance (XMLGenerator m, EmbedAsChild m XML) => (EmbedAsChild m RecentPaste) where
    asChild pe =
        <%
            <div class="recentpaste">
                <p class="paste-info"><a href=id'><% id' %></a> - <%
                    Description ids . fromMaybe "" $ rDesc pe
                %></p>
                <p class="paste-date">Pasted at: <% formatCalendarTime defaultTimeLocale "%H:%M - %a %Y.%m.%d" . toUTCTime $ rDate pe %></p>
                <pre><% "\n" ++ rCont pe %></pre>
            </div>
        %>
      where id  = unId $ rId pe
            ids = allIds pe
            id' = "/" ++ id ++ "/"
