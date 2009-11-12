module Paste.Control (pasteHandler) where

import Happstack.State
import Happstack.Server

import Control.Monad (msum)

import Paste.View (showPaste)
import Paste.ViewIndex (showIndex)
import Paste.Post (postHandler)

pasteHandler :: ServerPartT IO Response
pasteHandler = msum
    [ postHandler
    , dir "static" $ fileServe ["index.html"] "npaste.de"
    , path showPaste
    , showIndex
    ]
