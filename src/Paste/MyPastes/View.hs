{-# OPTIONS_GHC -F -pgmFtrhsx #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances #-}

module Paste.MyPastes.View
    ( showMyPastes
    ) where

import Data.Maybe (fromMaybe)
import Happstack.Server
import Happstack.State
import HSP
import System.Time
import System.Locale

import qualified Data.Set           as S

import Paste.View
import Paste.View.Pastes
import Paste.View.Recent (makeRecent, sortByDate, RecentPaste (..))
import Paste.State
import Util.Control

showMyPastes :: ServerPart Response
showMyPastes = do

    (uid,_) <- requireLogin
    pastes  <- query $ GetPastesByUser (Just uid)
    number  <- getNumber `fmap` getDataQueryFn (look "show")
    recent  <- mapM makeRecent . take number . sortByDate . S.toAscList $ pastes

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
               then <% <p>Nothing pasted yet!</p> %>
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
                <p class="paste-info"><a href=id'><% id' %></a> - <a href=edit>Edit</a> - <%
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
