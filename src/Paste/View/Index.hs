{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Paste.View.Index
    ( showIndex
    , showIndex'
    , indexBody
    ) where

import Data.List  (elemIndex)
import Data.Maybe (fromMaybe, isJust)

import HSP
import Happstack.Server
import Happstack.Server.HSP.HTML (webHSP)

import Text.Highlighting.Kate (languages)

import App.View
import Paste.Types              ( PostError (..) )
import Paste.State
    ( IDType (..)
    )


-- little helper :)
a +?+ "" = ""
a +?+ b  = a ++ b

-- options
htmlOpts = [ WithCss    $ CssFile "/static/style.css"
           , WithTitle  $ "npaste.de"
           ]

--------------------------------------------------------------------------------
-- ServerPartT stuff
--------------------------------------------------------------------------------

-- | show index
showIndex :: ServerPartT IO Response
showIndex = showIndex' Nothing
showIndex' err = do

    content     <- getDataBodyFn $ look "content"
    description <- getDataBodyFn $ look "description"
    idT         <- getDataBodyFn $ look "id-type"
    id          <- getDataBodyFn $ look "id"
    filetype    <- getDataBodyFn $ look "filetype"

    xmlResponse $ indexBody err content description filetype idT id



--------------------------------------------------------------------------------
-- HTML
--------------------------------------------------------------------------------

type ErrorMsg = Maybe String
type Content = Maybe String
type Description = Maybe String
type Filetype = Maybe String
type IdType = Maybe String
type Id = Maybe String

-- body
indexBody :: ErrorMsg -> Content -> Description -> Filetype -> IdType -> Id -> HtmlBody
indexBody err content description filetype idtype id = do

    HtmlBody htmlOpts $
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

                <form id="paste" action="/" method="post">
                    <%
                        if isJust err
                           then [<p class="error">Error: <% err %></p>]
                           else []
                    %>
                    <p>Description: <input type="text" name="description" id="description" value=(fromMaybe "" description)/></p>
                    <textarea name="content" rows="20" cols="80">
                        <% content %>
                    </textarea>
                    <p>
                        <select size="1" name="filetype">
                            <% map langSelect langOptions %>
                        </select>
                        <select size="1" name="id-type" selected="2">
                            <% map idSelect idTypeOptions %>
                        </select>
                        <input type="text" name="id" id="id" value=(fromMaybe "" id) />
                        <input type="submit" name="submit" id="submit"/>
                        <input type="text" style="display: none;" name="email" id="email"/> -- invisible anti-spam input
                    </p>
                </form>
            </div>
        ]

  where idTypeOptions = ["Default ID", "Random ID", "Custom ID"]
        idSelect l | l == (fromMaybe "" idtype) = <option selected="selected"><% l %></option>
                   | otherwise = <option><% l %></option>

        langOptions   = ("Plain text" : "Tiny Url" : optSeparator : languages ++ [optSeparator])
        langSelect l | l == (fromMaybe "" filetype) = <option selected="selected"><% l %></option>
                     | otherwise     = <option><% l %></option>

        optSeparator  = "----------"
