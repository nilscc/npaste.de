{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Paste.View.Index
    ( showIndex
    , showIndex'
    , indexHsp
    ) where

import Data.List                        (elemIndex)
import Data.Maybe                       (fromMaybe, isJust)

import HSP

import Happstack.Server
import Happstack.Server.HSP.HTML        (webHSP)
import Happstack.State                  (query)

import Text.Highlighting.Kate           (languages)

import App.View
import Paste.Types                      (PostError (..), LoggedIn (..))
import Paste.State                      (IDType (..))
import Paste.View                       (htmlOpts, getLogin)
import Paste.View.Menu                  (menuHsp)

import Users.State                      (UserOfSessionId (..))
import Users.State.SessionID            (SessionID (..))


-- little helper :)
a +?+ "" = ""
a +?+ b  = a ++ b

--------------------------------------------------------------------------------
-- ServerPartT stuff
--------------------------------------------------------------------------------

-- | show index
showIndex :: ServerPartT IO Response
showIndex = showIndex' Nothing

-- | showIndex with error message
showIndex' :: Maybe String -> ServerPartT IO Response
showIndex' err = do

    content     <- getDataBodyFn $ look "content"
    description <- getDataBodyFn $ look "description"
    idT         <- getDataBodyFn $ look "id-type"
    id          <- getDataBodyFn $ look "id"
    filetype    <- getDataBodyFn $ look "filetype"

    loggedInAs  <- getLogin

    xmlResponse $ HtmlBody htmlOpts [menuHsp loggedInAs, indexHsp err content description filetype idT id]



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
indexHsp :: ErrorMsg -> Content -> Description -> Filetype -> IdType -> Id -> HSP XML
indexHsp err content description filetype idtype id =
            <div id="main">
                <h1>New Paste</h1>
                <p>To add a new paste you can either get the <a href="/?view=download">client</a>, use curl with...
                    <pre><% "cat <file> | curl -F \"content=<-\" npaste.de" %></pre>
                    ...or enter your text below to add a new paste.</p>
                <form id="paste" action="/" method="post">
                    <%
                        if isJust err
                           then [<p class="error">Error: <% err %></p>]
                           else []
                    %>
                    Description: <input type="text" name="description" id="description" value=(fromMaybe "" description)/>
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
                        <input type="checkbox" name="hide" id="hide" value="hide" /> Hide from recent pastes
                        <input type="submit" name="submit" id="submit"/>
                        <input type="text" style="display: none;" name="email" id="email"/> -- invisible anti-spam input
                    </p>
                </form>
            </div>

  where idTypeOptions = ["Default ID", "Random ID", "Custom ID"]
        idSelect l | l == (fromMaybe "" idtype) = <option selected="selected"><% l %></option>
                   | otherwise = <option><% l %></option>

        langOptions   = ("Plain text" : "Tiny Url" : optSeparator : languages ++ [optSeparator])
        langSelect l | l == (fromMaybe "" filetype) = <option selected="selected"><% l %></option>
                     | otherwise     = <option><% l %></option>

        optSeparator  = "----------"
