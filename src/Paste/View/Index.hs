{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Paste.View.Index
    ( showIndex
    , showIndex'
    , indexHsp
    ) where

import Control.Monad
import Data.Maybe                       (fromMaybe, isJust)
import HSP
import Happstack.Server
import Happstack.State
import Text.Highlighting.Kate           (languages)

import qualified Data.Map as M

import Paste.View
import Users.State
import Util.Control

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

    pastesettings <- msum
        [ do
            (uid,_) <- requireLogin
            fmap defaultPasteSettings `fmap` query (UserDataByUserId uid)
        , return Nothing
        ]

    htmlBody [indexHsp err content description filetype idT id pastesettings]



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
indexHsp :: ErrorMsg -> Content -> Description -> Filetype -> IdType -> Id -> Maybe PasteSettings -> HSP XML
indexHsp err content description filetype idtype id pastesettings =
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
                    <p>Description: <input type="text" name="description" id="description" value=(fromMaybe "" description)/></p>
                    <textarea name="content" rows="20" cols="80">
                        <% content %>
                    </textarea>
                    <p>
                        <select size="1" name="filetype">
                            <% map langSelect langOptions %>
                        </select>
                        <select size="1" name="id-type" selected="2">
                            <% map idSelect $ map snd idTypeOptions %>
                        </select>
                        <input type="text" name="id" id="id" value=(fromMaybe "" id) />
                        <% case pastesettings of
                                Just DefaultPasteSettings -> <input type="checkbox" name="hide" id="hide" value="hide" />
                                _                         -> <input type="checkbox" name="hide" id="hide" value="hide" checked="checked" />
                            %> Hide from recent pastes
                        <input type="submit" name="submit" id="submit"/>
                        <input type="text" style="display: none;" name="email" id="email"/> -- invisible anti-spam input
                    </p>
                </form>
            </div>

  where idTypeOptions = [ (Just DefaultPasteSettings, "Default ID")
                        , (Just HideAndRandom, "Random ID")
                        , (Nothing, "Custom ID")
                        ]

        idSelect l
            -- Try to use default paste settings if idtype is Nothing or empty
            | not ((fromMaybe "" idtype) `elem` map snd idTypeOptions)
                && isJust pastesettings
                && Just l == M.lookup pastesettings (M.fromList idTypeOptions) =

                    <option selected="selected"><% l %></option>

            -- Otherwise compare:
            | l == (fromMaybe "" idtype) = <option selected="selected"><% l %></option>
            | otherwise                  = <option><% l %></option>

        langOptions   = ("Text" : "Render Markdown" : optSeparator : languages ++ [optSeparator])
        langSelect l | l == (fromMaybe "" filetype) = <option selected="selected"><% l %></option>
                     | otherwise     = <option><% l %></option>

        optSeparator  = "----------"
