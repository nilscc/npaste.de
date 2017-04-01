{-# LANGUAGE NamedFieldPuns #-}

module NPaste.Routes.Read
  ( readR, readRedirectR
  ) where

import Data.ByteString.Char8 (unpack)
import Happstack.Server

import NPaste.Database
import NPaste.Html
import NPaste.Types
import NPaste.Parser
import NPaste.Utils

-- TODO: handle /u/<user>/<id> urls

readR, readRedirectR :: String -> NPaste ()

readR pid | length pid >= 2 = showPasteR pid
readR _                     = mzero

readRedirectR pid | length pid >= 2 = do
  rq  <- askRq
  url <- msum [ trailingSlash >> return ("/p/" ++ pid ++ "/")
              , return ("/p/" ++ pid)
              ]
  PlainResponse rq .= seeOther url . toResponse $ "Paste moved to " ++ url
readRedirectR _                     = mzero

showPasteR :: Id -> NPaste ()
showPasteR pid = msum
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
       setLang   <- msum [ path $ \l -> return $
                             case findLang l of
                               Just "Plaintext" -> \p -> p{ pasteType = Nothing }
                               Just newLang     -> \p -> p{ pasteType = Just newLang }
                               _                -> id
                         , return id ]
       -- get all informations, set the language etc pp
       paste     <- fmap setLang `fmap` runQuery (getPasteById pid)
       repl      <-  map pasteId `fmap` runQuery (getReplies pid 20 0)
       muser     <- case paste of
                         Just (Paste{ pasteUserId = uid }) -> runQuery $ getUserById uid
                         _                                 -> return Nothing
       Title     .= Just $ "/" ++ pid ++ "/" ++ maybe "" ((" - " ++) . take 50 . descToString)
                                                      (join $ fmap pasteDescription paste)
       CSS       .= ["code.css", "read.css"]
       Script    .= ["read.js"]
       HtmlFrame .= compactFrame (readInfo paste muser repl)
       HtmlBody  .= readHtml paste
  , do paste <- runQuery $ getPasteById pid
       rq    <- askRq
       case paste of
            Just p  -> PlainResponse rq .= ok . toResponse . unpack $ pasteContent p
            Nothing -> PlainResponse rq .= notFound $ toResponse "Paste not found.\n"
  ]
 where
  checkPath     = join $ msum [ nullDir >> return trailingSlash, return (return ()) ]
