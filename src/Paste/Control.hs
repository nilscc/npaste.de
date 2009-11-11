module Paste.Control (pasteHandler) where

import Happstack.State
import Happstack.Server

import Control.Monad (msum)

import Paste.View (showPaste)
import Paste.Post (postHandler)

pasteHandler :: ServerPartT IO Response
pasteHandler = msum
    [ postHandler
    , dir "static" $ fileServe [] "npaste.de"
    , path showPaste
    , fileServe ["index.html"] "npaste.de"
    ]
