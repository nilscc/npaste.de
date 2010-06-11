{-# OPTIONS_GHC -F -pgmFtrhsx #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Paste.MyPastes.Remove
    ( removeMyPaste
    ) where

import Control.Monad
import Control.Monad.Trans
import Happstack.Server
import Happstack.State
import HSP
import System.Directory

import Paste.State
import Paste.View
import Util.Control

removeMyPaste :: ServerPart Response
removeMyPaste = do

    (uid,_) <- requireLogin
    pid     <- maybe mzero return =<< getDataQueryFn (look "remove")
    paste   <- maybe mzero return =<< query (GetPasteById $ ID pid)

    -- Exit here if paste ID is invalid
    when (unUser (user paste) /= Just uid) mzero

    confirm <- getDataQueryFn $ look "confirm"

    if confirm == Just "yes"
        then do

            s <- update $ RemovePaste (ID pid)
            if s
               then do
                   -- case content paste of
                        -- PContent (File fp) -> liftIO $ removeFile fp `Prelude.catch` \e ->
                                -- putStrLn $ "Error while removing /" ++ pid ++ "/: " ++ show e -- TODO: use decent logger :)
                        -- _                  -> return ()

                   -- update $ RemoveReplies (ID pid)
                   htmlBody [removeHspSuccess paste]
               else htmlBody [removeHspFail paste]

        else htmlBody [confirmHsp paste]


--------------------------------------------------------------------------------
-- HSP
--------------------------------------------------------------------------------

confirmHsp :: PasteEntry -> HSP XML
confirmHsp PasteEntry { pId = PId (ID id) } =

    <div id="main">
        <h1>Remove: <% idLink %></h1>
        <p><strong>Are you sure you want to remove Paste <% idLink %>? This cannot be undone.</strong></p>
        <p><a href=confirm>Remove</a> or <a href=back>Cancel</a>.</p>
    </div>

  where idLink  = <a href=("/"++ id ++"/")>/<% id %>/</a>
        confirm = "/?view=mypastes&remove=" ++ id ++ "&confirm=yes"
        back    = "/?view=mypastes"

confirmHsp p = removeHspFail p


removeHspSuccess :: PasteEntry -> HSP XML
removeHspSuccess PasteEntry { pId = PId (ID id) } =

    <div id="main">
        <h1>Remove: <% idL %></h1>
        <p>Paste <% idL %> got removed. Get back to <a href=back>my pastes</a>.</p>
    </div>

  where back = "/?view=mypastes"
        idL  = "/" ++ id ++ "/"

removeHspSuccess p = removeHspFail p


-- | Failed to remove Paste/invalid ID
removeHspFail :: PasteEntry -> HSP XML
removeHspFail PasteEntry { pId = PId (ID id) } =

    <div id="main">
        <h1 class="error">Error: Failed to remove /<% id %>/</h1>
        <p>Get back to <a href="/?view=mypastes">my pastes</a>.</p>
    </div>

removeHspFail _ =

    <div id="main">
        <h1 class="error">Error: Invalid ID</h1>
        <p>Get back to <a href="/?view=mypastes">my pastes</a>.</p>
    </div>
