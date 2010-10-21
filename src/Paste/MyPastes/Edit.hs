{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Paste.MyPastes.Edit
    ( editMyPaste
    ) where

import Control.Monad
import Control.Monad.Trans
import Codec.Binary.UTF8.Light
import Data.Maybe
import Data.Char
import Happstack.Server
import Happstack.State
import HSP
import Text.Highlighting.Kate (languages)

import Paste.State
import Paste.View
import Util.Control
import Util.IO

editMyPaste :: ServerPart Response
editMyPaste = msum

    [ methodM POST >> updatePaste
    , showPaste Nothing
    ]

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

--------------------------------------------------------------------------------
-- Update/edit paste
--------------------------------------------------------------------------------

updatePaste :: ServerPart Response
updatePaste = do

    (uid,_) <- requireLogin
    pid     <- either (const "") id `fmap` getDataFn (queryString $ look "edit")

    -- Look for "remove" button
    remove  <- getDataFn . body $ (look "remove")
    if remove == Right "Remove"
       then seeOther ("/?view=mypastes&remove=" ++ pid) (toResponse $ "Forward to \"My Pastes\" -> \"Remove /" ++ pid ++ "/\"")
       else do

           -- Get the paste entry
           paste   <- maybe mzero return =<< query (GetPasteById $ ID pid)

           when (unUser (user paste) /= Just uid) mzero

           -- Get POST values
           desc    <- either (const "") id          `fmap` (getDataFn . body $ look "description")
           cont    <- either (const "") stripSpaces `fmap` (getDataFn . body $ look "content")
           ft      <- either (const "") id          `fmap` (getDataFn . body $ look "filetype")
           hide    <- isRight                       `fmap` (getDataFn . body $ look "hide")

           case cont of

                _ | null cont || all isSpace cont ->

                       showPaste $ Just <p class="error">No content given.</p>

                  | not . null $ drop (200 * 1000) cont ->

                       showPaste $ Just <p class="error">Content too big.</p>

                  | otherwise -> do

                       -- Update file/plaintext content
                       cont' <- case unPContent $ content paste of
                                     File fp -> liftIO (writeUTF8File fp $ encode cont) >> return (File fp)
                                     Plain _ -> return $ Plain cont

                       -- TODO: Remove old replies, add new replies
                       -- update $ RemoveReplies (ID pid)
                       -- update $ 

                       update $ UpdatePaste (ID pid) paste
                           { description = PDescription (if null desc then Nothing else Just desc)
                           , content     = PContent cont' -- $ Plain cont
                           , filetype    = PFileType (if null ft then Nothing else Just ft)
                           , hide        = PHide hide
                           }

                       showPaste $ Just <p class="success">Paste updated.</p>


--------------------------------------------------------------------------------
-- Show paste
--------------------------------------------------------------------------------

showPaste :: Maybe (HSP XML) -> ServerPart Response
showPaste info = do

    (uid,_) <- requireLogin
    pid     <- either (const "") id `fmap` getDataFn (queryString $ look "edit")
    paste   <- maybe mzero return =<< query (GetPasteById $ ID pid)

    -- Exit here if paste ID is invalid
    when (unUser (user paste) /= Just uid) mzero

    cont <- case unPContent $ content paste of
                 File fp -> liftIO $ readFile' fp
                 Plain t -> return t

    htmlBody [editHsp info paste cont]

editHsp :: Maybe (HSP XML)  -- ^ Information
        -> PasteEntry
        -> String           -- ^ Content (read from file)
        -> HSP XML
editHsp info pe cont =

    <div id="main">
        <h1>Edit: <% idLink %></h1>
        <% info %>
        <form id="paste" action=("/?view=mypastes&edit=" ++ id) method="post">
            <p>Description: <input type="text" name="description" id="description" value=(fromMaybe "" (unPDescription $ description pe))/></p>
            <textarea name="content" rows="20" cols="80">
                <% cont %>
            </textarea>
            <p>
                <select size="1" name="filetype">
                    <% map langSelect langOptions %>
                </select>
                -- <select size="1" name="id-type" selected="2">
                    -- <% map idSelect $ map snd idTypeOptions %>
                -- </select>
                -- <input type="text" name="id" id="id" value=(fromMaybe "" id) />
                <% if unPHide $ hide pe
                      then <input type="checkbox" name="hide" id="hide" value="hide" checked="checked" />
                      else <input type="checkbox" name="hide" id="hide" value="hide" />
                    %> Hide from recent pastes
                <input type="submit" name="submit" id="submit" value="Save" />
                <input type="submit" name="remove" id="remove" value="Remove" />
            </p>
        </form>
    </div>

  where idLink = <a href=("/"++ id ++"/")>/<% id %>/</a>
        id = (unId . unPId . pId) pe

        langOptions   = ("Text" : "Render Markdown" : optSeparator : languages ++ [optSeparator])
        langSelect l
            | l == (fromMaybe "" (unPFileType $ filetype pe)) =
                <option selected="selected"><% l %></option>
            | otherwise = <option><% l %></option>
        optSeparator  = "----------"
