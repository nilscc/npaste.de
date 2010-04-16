module Paste.Control (pasteHandler) where

import Happstack.Server

import Data.Char            (toLower)
import Control.Monad

import Paste.View.Download  (showDownload)
import Paste.View.Pastes    (showPaste)
import Paste.View.Index     (showIndex)
import Paste.View.Recent    (showRecent)
import Paste.View.Faq       (showFaq)
import Paste.View.Info      (showInfo)
import Paste.Post.NewPaste

import Paste.Login.Control
import Paste.Register.Control
import Paste.Profile.Control
import Paste.MyPastes.Control

import Util.Control

pasteHandler :: ServerPartT IO Response
pasteHandler = msum
    [ dir "static" $ fileServe ["index.html"] "npaste.de"
    , dir "client" $ fileServe ["index.html"] "npaste.de/client"
    , path showPaste
    , hQuery "view" viewHandler
    , newPasteHandler
    , showIndex
    ]


viewHandler :: String -> ServerPart Response
viewHandler s
    | view == "recent"      = showRecent
    | view == "download"    = showDownload
    | view == "faq"         = showFaq
    | view == "info"        = showInfo

    | view == "register"    = withoutLogin  registerControl
    | view == "login"       = withoutLogin  loginControl

    | view == "logout"      = withLogin     logoutControl
    | view == "profile"     = withLogin     profileControl
    | view == "mypastes"    = withLogin     myPastesControl

    | otherwise             = showIndex
  where view = map toLower s
