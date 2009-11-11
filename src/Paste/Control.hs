module Paste.Control (pasteHandler) where

import Happstack.State
import Happstack.Server

import Control.Monad (msum)

import Paste.View (showPaste)
import Paste.Post (postHandler)
import Paste.State (GetNewId(..))

pasteHandler :: ServerPartT IO Response
pasteHandler = msum
    [ postHandler
    , dir "static" $ fileServe [] "npaste.de"
    , dir "show" $ showInformation
    , path showPaste
    , fileServe ["index.html"] "npaste.de"
    ]

showInformation = do
    paste <- query $ GetNewId
    ok . toResponse $ show paste
