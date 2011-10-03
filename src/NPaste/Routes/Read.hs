{-# LANGUAGE NamedFieldPuns #-}

module NPaste.Routes.Read where

import Happstack.Server

import NPaste.Database
import NPaste.Html
import NPaste.Types


readR :: String -> ServerPart Response
readR pId | length pId >= 2 = do
  let pid = ID pId
  paste <- getPasteById pid
  repl  <- getReplies   pid
  return . toResponse . compactFrame (readInfo paste repl) $ nullBody
    { css    = ["code/hk-pyg.css", "code.css", "read.css"]
    , html   = readHtml paste
    }
readR "r" = do
  pastes   <- getRecentPastes Nothing 20 0 False
  return . toResponse . mainFrame $ nullBody
    { css    = ["code/hk-pyg.css", "code.css", "recent.css"]
    , html   = recentHtml $ pastes
    }
readR _ = mzero
