{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Paste.View.Faq
    ( showFaq
    ) where

import HSP
import Happstack.Server
import Text.Pandoc          (readMarkdown, defaultParserState)

import App.View             (xmlResponse, HtmlBody (..), pandocToXml)
import Paste.View           (htmlOpts, getLogin)
import Paste.View.Menu      (menuHsp)
import Paste.Types          (LoggedIn (..))

faqs :: [Faq]
faqs =
 [ Faq "What features are planed for npaste.de?"
 "Current goal is to implement users, so you can delete and change your pastes. Other planed features are:

 * Users"

 , Faq "How long will my pastes exist?"
   "Currently, npaste.de does not delete any pastes at all. However, this could change in future, depending on the success of npaste.de."

 , Faq "How can I reply to a paste?" $
   "Just add an \"/id/\" to your pastes description. npaste.de will then automaticly detect "
   ++ "if there is a valid paste with that ID and add a \"Replies: ...\" link to it."
 ]

showFaq = do
    loggedInAs  <- getLogin
    xmlResponse $ HtmlBody htmlOpts [menuHsp loggedInAs, faqHsp]

data Faq = Faq { question :: String
               , answer   :: String
               }

instance (XMLGenerator m, EmbedAsChild m XML) => (EmbedAsChild m Faq) where
    asChild faq =
        <%
            [ <p class="faq-question"><% question faq %></p>
            , <p class="faq-answer"><% pandocToXml . readMarkdown defaultParserState $ answer faq %></p>
            ]
        %>

faqHsp =
    <div id="main">
        <h1>FAQ - Frequently Asked Questions</h1>
        <div id="faq">
            <% faqs %>
        </div>
    </div>
