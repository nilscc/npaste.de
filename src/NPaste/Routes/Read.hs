{-# LANGUAGE NamedFieldPuns #-}

module NPaste.Routes.Read
  ( readR
  ) where

import Data.ByteString.Char8 (unpack)
import Data.Char
import Happstack.Server
import Text.Highlighting.Kate

import NPaste.Database
import NPaste.Html
import NPaste.State
import NPaste.Types
import NPaste.Parser


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
            (newLang:_) -> do
              let url = "/" ++ pid ++ "/" ++ newLang
              rq <- askRq
              PlainResponse rq .= seeOther url . toResponse $ "Please go to npaste.de" ++ url
            [] -> mzero
  , do -- See if we have a trailing slash if there is no data following
       checkPath
       -- See if the URL contains some language informations
       setLang   <- choice [ path $ \l -> return $
                                 case findLang l of
                                      ("Plaintext":_) -> \p -> p{ pasteType = Nothing }
                                      (newLang:_)     -> \p -> p{ pasteType = Just newLang }
                                      _               -> id
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
       maybe mzero ((.=) (PlainResponse rq) . return . toResponse . unpack . pasteContent) paste
  ]
 where
  checkPath     = join $ choice [ nullDir >> return trailingSlash, return (return ()) ]
  findLang lang = [ l | l <- "Plaintext" : languages
                      , map toLower lang == map toLower l]
                  ++ languagesByExtension lang
                  ++ languagesByFilename lang
