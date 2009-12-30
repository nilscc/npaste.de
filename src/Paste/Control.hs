module Paste.Control (pasteHandler) where

import Happstack.State
import Happstack.Server

import Data.Char            (toLower)
import Control.Monad        (msum, mzero, MonadPlus)

import Paste.View.Download  (showDownload)
import Paste.View.Pastes    (showPaste)
import Paste.View.Index     (showIndex)
import Paste.View.News      (showNews)
import Paste.View.Recent    (showRecent)
import Paste.View.Faq       (showFaq)
import Paste.View.Register  (showRegister)
import Paste.View.Info      (showInfo)

import Paste.Post.NewPaste  (newPasteHandler)

pasteHandler :: ServerPartT IO Response
pasteHandler = msum
    [ dir "static" $ fileServe ["index.html"] "npaste.de"
    , dir "client" $ fileServe ["index.html"] "npaste.de/client"
    , path showPaste
    , hQuery "view" viewHandler
    , newPasteHandler
    , showIndex
    ]


viewHandler s
    | view == "recent"      = showRecent
    | view == "download"    = showDownload
    | view == "faq"         = showFaq
    | view == "info"        = showInfo
    -- | view == "register"    = showRegister
    -- | view == "login"       = showLogin
    -- | view == "news"        = showNews
    | otherwise             = showIndex
  where view = map toLower s


-- | Match on a QUERY element and pass its value to the function
hQuery :: (ServerMonad m, MonadPlus m) => String -> (String -> m a) -> m a
hQuery q f = do
    val <- getDataQueryFn $ look q
    case val of
         Just v | not (null v) -> f v
         _ -> mzero
