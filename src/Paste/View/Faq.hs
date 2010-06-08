{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, UndecidableInstances #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Paste.View.Faq
    ( showFaq
    ) where

import HSP
import Happstack.Server
import Paste.View

showFaq :: ServerPart Response
showFaq = htmlBody [faqHsp]

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
                If you think that support for some language/script is missing, feel free to <a href="/?view=info">contact me</a>.</p>
        </div>
    </div>
