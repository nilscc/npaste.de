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
import NPaste.Parser
import NPaste.Utils

-- TODO: handle /u/<user>/<id> urls

readR :: String -> NPaste ()
readR pid | length pid >= 2 = showPasteR pid
readR _                     = mzero

showPasteR :: Id -> NPaste ()
showPasteR pid = choice
  [ do methodM POST
       decodeBody (defaultBodyPolicy "/tmp/" 0 100000 100000)
       lang <- body $ look "lang"
       case findLang lang of
            Just newLang -> do
              let url = "/" ++ pid ++ "/" ++ newLang
              rq <- askRq
              PlainResponse rq .= seeOther url . toResponse $ "Please go to npaste.de" ++ url
            _ -> mzero
  , do -- See if we have a trailing slash if there is no data following
       checkPath
       -- See if the URL contains some language informations
       setLang   <- choice [ path $ \l -> return $
                                 case findLang l of
                                      Just "Plaintext" -> \p -> p{ pasteType = Nothing }
                                      Just newLang     -> \p -> p{ pasteType = Just newLang }
                                      _                -> id
                           , return id ]
       -- get all informations, set the language etc pp
       paste     <- fmap setLang `fmap` getPasteById pid
       repl      <-  map pasteId `fmap` getReplies pid 20 0
       Title     .= Just $ "/" ++ pid ++ "/" ++ maybe "" ((" - " ++) . take 50 . descToString)
                                                      (join $ fmap pasteDescription paste)
       CSS       .= ["code/hk-pyg.css", "code.css", "read.css"]
       Script    .= ["read.js"]
       HtmlFrame .= compactFrame (readInfo paste repl)
       HtmlBody  .= readHtml paste
  , do paste <- getPasteById pid
       rq    <- askRq
       case paste of
            Just p  -> PlainResponse rq .= ok . toResponse . unpack $ pasteContent p
            Nothing -> PlainResponse rq .= notFound $ toResponse "Paste not found.\n"
  ]
 where
  checkPath     = join $ choice [ nullDir >> return trailingSlash, return (return ()) ]
