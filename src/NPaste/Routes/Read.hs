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


readR :: String -> NPaste ()

-- handle /<id> urls
readR pid | length pid >= 2 = showPasteR pid

-- TODO: handle /u/<user>/<id> urls

-- readR "u" =
--   path $ \un -> do
--   guard $ not (null un)
--   mu <- getUserByName un
--   case mu of
--        Just u -> msum
--          [ path $ \pid -> do
--            showPasteR $ PrivateID u pid
--          , showRecentR (Just u) 20 0 False
--          ]
--        _ -> reset

-- show recent pastes
readR "r" = showRecentR Nothing 20 0 False

readR _ = reset


showPasteR :: Id -> NPaste ()
showPasteR pid = msum
  [ do -- see if we have a trailing slash if there is no data following
       checkPath
       paste       <- getPasteById pid
       repl        <- map pasteId `fmap` getReplies pid 20 0
       HtmlContext .= nullContext { css = CSS ["code/hk-pyg.css", "code.css", "read.css"] }
       HtmlFrame   .= compactFrame (readInfo paste repl)
       HtmlBody    .= readHtml paste
  , do paste <- getPasteById pid
       maybe reset (setNP . PlainResponse . toResponse . unpack . pasteContent) paste
  ]
 where
  checkPath = join $ msum [ nullDir >> return trailingSlash, return (return ()) ]

showRecentR :: Maybe User -> Int -> Int -> Bool -> NPaste ()
showRecentR mu l o hidden = do
  pastes      <- getRecentPastes mu l o hidden
  HtmlContext .= nullContext { css = CSS ["code/hk-pyg.css", "code.css", "recent.css"] }
  HtmlFrame   .= mainFrame
  HtmlBody    .= recentHtml pastes
