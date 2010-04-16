{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Paste.View.Faq
    ( showFaq
    ) where

import HSP
import Text.Pandoc          (readMarkdown, defaultParserState)
import Happstack.Server

import Paste.View

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

 , Faq "Missing highlight?" $
   "This pastebin is using the kate highlighting engine. If you think that npaste.de is missing "
   ++ "some highlight which is available at [kate-editor.org]"
   ++ "(http://kate-editor.org/downloads/syntax_highlighting?kateversion=3.2) feel free to "
   ++ "email me: <mail@n-sch.de>"

 ]

showFaq :: ServerPart Response
showFaq = htmlBody [faqHsp]

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

faqHsp :: HSP XML
faqHsp =
    <div id="main">
        <h1>FAQ - Frequently Asked Questions</h1>
        <div id="faq">

            <h2>How long will my pastes exist?</h2>
            <p>Currently, npaste.de does not delete any pastes at all. However, this could change in future, depending on the success of npaste.de.</p>

            <h2>How can I reply to a paste?</h2>
            <p>Just add an "/id/" to your paste description. Npaste.de will then automaticly detect if there is a valid paste with that ID and add a "Replies: ..." link to it.</p>

            <h2>Missing highlight?</h2>
            <p>npaste.de is using the <a href="http://kate-editor.org/downloads/syntax_highlighting?kateversion=3.2">Kate highlighting</a> engine.
                If you think that support for some language/script is missing, feel free to contact me.</p>
        </div>
    </div>
