{-# LANGUAGE NamedFieldPuns #-}

module NPaste.Routes.Read
  ( readR
  ) where

import Data.ByteString.Char8 (unpack)
import Happstack.Server

import NPaste.Database
import NPaste.Html
import NPaste.State
import NPaste.Types


-- TODO: handle /u/<user>/<id> urls

readR :: String -> NPaste ()
readR pid | length pid >= 2 = showPasteR pid
readR "r" = showRecentR Nothing 20 0 False
readR _   = mzero

showPasteR :: Id -> NPaste ()
showPasteR pid = choice
  [ do checkPath -- see if we have a trailing slash if there is no data following
       paste       <- getPasteById pid
       repl        <- map pasteId `fmap` getReplies pid 20 0
       HtmlContext .= nullContext { css = CSS ["code/hk-pyg.css", "code.css", "read.css"] }
       HtmlFrame   .= compactFrame (readInfo paste repl)
       HtmlBody    .= readHtml paste
  , do paste <- getPasteById pid
       maybe mzero (setNP . PlainResponse . toResponse . unpack . pasteContent) paste
  ]
 where
  checkPath = join $ choice [ nullDir >> return trailingSlash, return (return ()) ]

showRecentR :: Maybe User -> Int -> Int -> Bool -> NPaste ()
showRecentR mu l o hidden = do
  pastes      <- getRecentPastes mu l o hidden
  -- set menu location
  setNP M_Recent
  HtmlContext .= nullContext { css = CSS ["code/hk-pyg.css", "code.css", "recent.css"] }
  HtmlFrame   .= mainFrame
  HtmlBody    .= recentHtml pastes
