{-# OPTIONS_GHC -F -pgmFtrhsx #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances #-}

module Paste.MyPastes.View
    ( showMyPastes
    ) where

import Data.Maybe (fromMaybe)
import Happstack.Server
import HSP
import System.Time
import System.Locale
import qualified Happstack.Auth     as Auth

import Paste.View
import Paste.View.Pastes
import Paste.Recent
import Paste.State
import Util.Control

showMyPastes :: ServerPart Response
showMyPastes = do

    (_,Auth.Username un) <- requireLogin
    number  <- getNumber `fmap` getDataQueryFn (look "show")
    recent  <- getRecentPastes (Just un) False (Just number)

    htmlBody [myPastesHsp recent number]

  where getNumber :: Maybe String -> Int
        getNumber Nothing  = 20
        getNumber (Just s) = case reads s of
                                  ((i,_):_) | i >= 1 -> i
                                  _                  -> 20


myPastesHsp :: [RecentPaste] -> Int -> HSP XML
myPastesHsp entries num =

    <div id="main">
        <h1>My pastes</h1>
        <p>This page is only visible to you. To change or remove a paste click on "Edit" next to its ID.
            To view the full paste with syntax highlight click on its ID.</p>
        <%
            if null entries
               then <% <p>Nothing pasted yet. Add your <a href="/">first paste</a>!</p> %>
               else <% map MyRecentPaste entries %>
        %>
        <p class="moreless">View <a href=more>more</a> / <a href=less>less</a>.</p>
    </div>

  where more = "/?view=mypastes&show=" ++ show (num + 20)
        less | num > 20  = "/?view=mypastes&show=" ++ show (num - 20)
             | otherwise = "/?view=mypastes&show=" ++ show num

--------------------------------------------------------------------------------
-- XML instances
--------------------------------------------------------------------------------

-- h4x!
newtype MyRecentPaste = MyRecentPaste RecentPaste

instance (XMLGenerator m, EmbedAsChild m XML) => (EmbedAsChild m MyRecentPaste) where
    asChild (MyRecentPaste pe) =
        <%
            <div class="recentpaste">
                <p class="paste-info"><a href=id'><% id' %></a> <% if rHide pe then [<i>(hidden)</i>] else []
                    %> - <a href=edit>Edit</a>/<a href=remove>Remove</a> - <%
                    Description ids . fromMaybe "" $ rDesc pe
                %></p>
                <p class="paste-date">Pasted at: <% formatCalendarTime defaultTimeLocale "%H:%M - %a %Y.%m.%d" . toUTCTime $ rDate pe %></p>
                <pre><% ("\n" ++) . unlines . take 16 . lines $ rCont pe %></pre>
            </div>
        %>
      where id  = unId $ rId pe
            ids = allIds pe
            id' = "/" ++ id ++ "/"
            edit = "/?view=mypastes&edit=" ++ id
            remove = "/?view=mypastes&remove=" ++ id
