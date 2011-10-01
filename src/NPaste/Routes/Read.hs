{-# LANGUAGE NamedFieldPuns #-}

module NPaste.Routes.Read where

import Happstack.Server

import NPaste.Database
import NPaste.Html
import NPaste.Types


readR :: String -> ServerPart Response
readR pId | length pId >= 2 = do
  let mu = Nothing -- TODO
  pinfo <- getPasteById mu pId
  pcont <- getContent   mu pId
  repl  <- maybe (return []) getReplies pinfo
  return . toResponse . compactFrame (readInfo pinfo repl) $ nullBody
    { css    = ["code/hk-pyg.css", "code.css", "read.css"]
    , html   = readHtml pinfo pcont
    }
readR "r" = do
  pastes   <- getRecentPastes Nothing 20 0 False
  contents <- forM pastes $ \PasteInfo{ p_id, p_user_id } -> do
                mu <- getUserById p_user_id
                getContent mu p_id
  return . toResponse . mainFrame $ nullBody
    { css    = ["code/hk-pyg.css", "code.css", "recent.css"]
    , html   = recentHtml $ zip pastes contents
    }
readR _ = mzero
