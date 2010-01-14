{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Paste.View.Recent
    ( showRecent
    ) where

import Control.Monad            (liftM2)
import Data.List                (sortBy)
import Data.Maybe
import qualified Data.Set as S

import HSP
import Control.Monad.Trans      (liftIO)
import System.Time              (ClockTime (..), toUTCTime, calendarTimeToString)

import Happstack.Server
import Happstack.State          (query)

import App.View
import Users.State
import Paste.View               (htmlOpts, getLogin)
import Paste.View.Menu          (menuHsp)
import Paste.View.Pastes
import Paste.Types              (LoggedIn (..))
import Paste.State


showRecent = do
    loggedInAs  <- getLogin
    pastes      <- query $ GetAllEntries

    let
        getLast :: Int -> [a] -> [a]
        getLast n l = fst . flip (foldr `flip` ([],0)) l $ \ pe rest@(ls, x) -> if x < n then (ls ++ [pe], x+1) else rest

        sortByDate = sortBy $ \p1 p2 -> (date p2) `compare` (date p1)

    recent      <- mapM makeRecent . take 5 . sortByDate . S.toAscList . S.filter (not . unPHide . hide) $ pastes

    xmlResponse $ HtmlBody htmlOpts [menuHsp loggedInAs, recentHsp recent Nothing]



--------------------------------------------------------------------------------
-- Data definitions + Helper stuff
--------------------------------------------------------------------------------

data RecentPaste = RecentPaste { rDate      :: ClockTime
                               , rCont      :: String
                               , rDesc      :: Maybe String
                               , rId        :: ID
                               , allIds     :: S.Set ID
                               }

makeRecent :: PasteEntry -> ServerPart RecentPaste
makeRecent pe = do
    ids <- query $ GetAllIds
    content <- liftIO $ case unPContent (content pe) of
                             Plain str -> return str
                             File fp   -> readFile fp
    return $ RecentPaste { rDate  = unPDate $ date pe
                         , rCont  = content
                         , rDesc  = unPDescription $ description pe
                         , rId    = unPId $ pId pe
                         , allIds = ids
                         }


--------------------------------------------------------------------------------
-- HSP
--------------------------------------------------------------------------------

recentHsp :: [RecentPaste] -> Maybe String -> HSP XML
recentHsp entries user =
    <div id="main">
        <h1>Recent pastes<% maybe "" (" of " ++) user %>:</h1>
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
                <p class="paste-date">Pasted at: <% calendarTimeToString . toUTCTime $ rDate pe %></p>
                <pre><% ("\n" ++) . unlines . take 8 . lines $ rCont pe %></pre>
            </div>
        %>
      where id  = unId $ rId pe
            ids = allIds pe
            id' = "/" ++ id ++ "/"
