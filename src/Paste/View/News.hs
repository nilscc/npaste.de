{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Paste.View.News
    ( showNews
    ) where


import HSP
import Happstack.Server

import System.Time

import App.View                         (xmlResponse)
import Paste.View                       (htmlBody, getLogin)
import Paste.State
-- import Users.State                      (UserOfLogin (..))

showNews :: ServerPart Response
showNews = do

    htmlBody [<h1>An error occurred: No news yet</h1>]


--------------------------------------------------------------------------------
-- XML instances
--------------------------------------------------------------------------------

instance (XMLGenerator m, EmbedAsChild m XML) => (EmbedAsChild m [PasteEntry]) where
    asChild [] = <% <p>No news yet.</p> %>
    asChild pe =
        <%
            map `flip` pe $ \entry -> <p class="date">Date: <% calendarTimeToString . toUTCTime . unPDate $ date entry %></p>
        %>
