{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Paste.View.News
    ( showNews
    ) where


import Data.Maybe                   (fromMaybe)
import HSP
import Happstack.Server
import Text.Pandoc                  (readMarkdown, defaultParserState, ParserState (..))
import System.Time
import System.Locale

import App.View
import Paste.View
import Paste.Recent
import Paste.State

showNews :: ServerPart Response
showNews = do

    -- Load pastes
    pastes <- getRecentPastes (Just "news") True Nothing

    -- Get number of pastes to display
    show <- getNumber 20 `fmap` getDataQueryFn (look "show")

    htmlBody [newsHsp pastes show]

  where getNumber :: Int -> Maybe String -> Int
        getNumber def Nothing  = def
        getNumber def (Just s) = case reads s of
                                      ((i,_):_) | i >= 1 -> i
                                      _                  -> def


--------------------------------------------------------------------------------
-- HSP
--------------------------------------------------------------------------------

newsHsp :: [RecentPaste] -> Int -> HSP XML
newsHsp [] _ =

    <div id="main">
        <h1>News</h1>
        <p>No news yet!</p>
    </div>

newsHsp l n =

    <div id="main">
        <h1>News</h1>
        <% map renderNews l %>
        <p class="moreless">View <a href=more>more</a> / <a href=less>less</a>.</p>
    </div>

  where more = "/?view=news&show=" ++ show (n + 20)
        less | n > 20    = "/?view=news&show=" ++ show (n - 20)
             | otherwise = "/?view=news&show=" ++ show n


renderNews :: RecentPaste -> HSP XML
renderNews pe@RecentPaste { rCont = content, rId = ID id } =

    <div class="news" id=(unId $ rId pe)>
        <p class="news-info"><a href=("/?view=news#" ++ id)><% fromMaybe "No description." (rDesc pe) %></a></p>

        <p class="news-date">News from: <% formatCalendarTime defaultTimeLocale "%H:%M - %a %Y.%m.%d" . toUTCTime $ rDate pe %></p>

        <% if rFiletype pe `elem` map Just ["Render Markdown", "render-markdown"]
              then  <div class="news-rendered"><%
                        pandocToXml $ readMarkdown defaultParserState { stateSanitizeHTML = True } content
                    %></div>
              else  <pre><% content %></pre>
        %>
    </div>

renderNews pe = renderNews pe { rId = ID "" }
