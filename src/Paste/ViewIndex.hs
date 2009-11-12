{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Paste.ViewIndex (showIndex) where

import HSP
import Happstack.Server
import Happstack.Server.HSP.HTML (webHSP)

import Text.Highlighting.Kate (languages)

showIndex :: ServerPartT IO Response
showIndex = webHSP $ index

index :: HSP XML
index =
    <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="de" lang="de">
        <head>
            <title>npaste.de</title>
            <meta http-equiv="content-type" content="text/html; charset=utf-8" />
            <link href="/static/style.css" type="text/css" rel="stylesheet" />
        </head>
        <body>

            <div id="main">
                <h1>nPaste.de - Pastebin on <a href="http://www.n-sch.de">n-sch.de</a></h1>

                <p>This pastebin is running on happstack. You can use it with a simple curl command:</p>

                <pre><% "cat <file>| curl -F \"content=<-\" -F \"filetype=<filetype>\" npaste.de" %></pre>

                <p>Where <% "\"<filetype>\"" %> specifies your default highlighting language (optional).</p>

                <p>Or simply enter your paste below:</p>

                <form id="paste" action="http://npaste.de" method="post">
                    <textarea name="content" rows="20" cols="80"></textarea>
                    <p>Language:
                        <select size="1" name="filetype">
                            <% map option ("Plain text" : languages) %>
                        </select>
                        <input type="submit" name="submit" />
                        <input type="reset" />
                    </p>
                </form>
            </div>

        </body>
    </html>

  where option l = <option><% l %></option>
