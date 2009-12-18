module Paste.Control (pasteHandler) where

import Happstack.State
import Happstack.Server

import Control.Monad        (msum)

import Paste.View.Login     (showLogin, showRegister)
import Paste.View.Pastes    (showPaste)
import Paste.View.Index     (showIndex)
import Paste.Post           (postHandler)

pasteHandler :: ServerPartT IO Response
pasteHandler = msum
    [ postHandler
    -- , dir "login"       showLogin
    -- , dir "register"    showRegister
    , dir "static" $ fileServe ["index.html"] "npaste.de"
    , dir "client" $ fileServe ["index.html"] "npaste.de/client"
    , path showPaste
    , showIndex
    ]
