{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Paste.View.Index
    ( showIndex
    , showIndex'
    , indexBody
    ) where

import Data.List  (elemIndex)
import Data.Maybe (fromMaybe)

import HSP
import Happstack.Server
import Happstack.Server.HSP.HTML (webHSP)

import Text.Highlighting.Kate (languages)

import App.View
import Paste.Types
    ( ShowOnIndex (..)
    , PasteResponse (..)
    , PostData (..)
    )
import Paste.State
    ( IDType (..)
    )


--------------------------------------------------------------------------------
-- ServerPartT stuff
--------------------------------------------------------------------------------

-- | show index
showIndex :: ServerPartT IO Response
showIndex = xmlResponse . indexBody $ ShowOnIndex Nothing Nothing


-- | show index without ShowOnIndex
showIndex' :: ShowOnIndex -> ServerPartT IO Response
showIndex' = xmlResponse . indexBody


--------------------------------------------------------------------------------
-- HTML
--------------------------------------------------------------------------------

-- little helper :)
a +?+ "" = ""
a +?+ b  = a ++ b

-- options
htmlOpts = [ WithCss    $ CssFile "/static/style.css"
           , WithTitle  $ "npaste.de"
           ]

-- body
indexBody showOnIndex = HtmlBody htmlOpts $
        [
            <div id="main">
                <h1>npaste.de - Pastebin on <a href="http://www.n-sch.de">n-sch.de</a></h1>

                <p>This pastebin is running on happstack. You can use it with a simple curl command:</p>

                <pre><% "cat <file>| curl -F \"content=<-\" -F \"filetype=<filetype>\" npaste.de" %></pre>

                <p>Where <% "\"<filetype>\"" %> specifies your default highlighting language (optional).</p>
                <p>
                    You can also get one of the linux clients
                    (<a href="http://npaste.de/client/32bit/np">32bit</a> and <a href="http://npaste.de/client/64bit/np">64bit</a>,
                    compiled on Arch Linux).
                    The source code is available at <a href="http://npaste.de/clientsrc/">#clientsrc</a>.
                </p>

                <p>Or simply enter your paste below:</p>

                <form id="paste" action="http://npaste.de" method="post">
                -- <form id="paste" action="http://localhost" method="post">
                    <%
                        -- Show error if any
                        case pasteResponse showOnIndex of
                             Nothing          -> []
                             Just (NoError _) -> []
                             Just err         -> [<p class="error">Error: <% show err %></p>]
                    %>
                    <textarea name="content" rows="20" cols="80">
                        <%
                            -- Load postData content
                            maybe "" cont (postData showOnIndex)
                        %>
                    </textarea>
                    <p>
                        <select size="1" name="filetype">
                            <%
                                map option $ maybe langOptions
                                                   (\n -> take (length langOptions) . drop n $ cycle langOptions)
                                                   (do pd       <- postData showOnIndex
                                                       filetype <- ft pd
                                                       elemIndex filetype langOptions
                                                   )
                                                       
                            %>
                        </select>
                        <select size="1" name="id-type" selected="2">
                            <%
                                map option $ maybe idTypeOptions
                                                   (\pd ->
                                                       let n = case idType pd of
                                                                    DefaultID   -> 0
                                                                    RandomID _  -> 1
                                                                    CustomID _  -> 2
                                                       in take 3 . drop n $ cycle idTypeOptions
                                                   )
                                                   (postData showOnIndex)
                            %>
                        </select>
                        <input type="text" name="id" id="id" value="" />
                        <input type="submit" name="submit" id="submit"/>
                        <input type="text" style="display: none;" name="email" id="email"/> -- invisible anti-spam input
                    </p>
                </form>
            </div>
        ]

  where option l = <option><% l %></option>
        idTypeOptions = ["Default ID", "Random ID", "Custom ID"]
        langOptions   = ("Plain text" : "Tiny Url" : optSeparator : languages ++ [optSeparator])
        optSeparator  = "----------"
