{-# LANGUAGE NamedFieldPuns #-}

module NPaste.Routes.Index where

import Happstack.Server
import Data.ByteString.Char8 (pack)
import Data.Char
import Data.Maybe
import Text.Highlighting.Kate

import NPaste.Database
import NPaste.Html
import NPaste.Types
import NPaste.State


indexR :: NPaste ()
indexR = do

  -- set menu location
  setNP M_AddNewPaste

  pdata <- choice
    [ methodM POST >> getIndexPostData
    , return nullPostData ]

  r <- if pdata == nullPostData then
         return $ Left Nothing
        else do
         let filetype  = lookupLanguage $ getValue pdata "lang"
             desc      = case getValue pdata "desc" of
                              d | null d    -> Nothing
                                | otherwise -> Just d
             hidden    = getValue pdata "hidden" == "on"
             content   = pack $ cutTrailingSpaces $ getValue pdata "content"
             spam      = not . null $ getValue pdata "email" -- should always be null!
             asReply   = not . null $ getValue pdata "asreply"

         when spam $ error "Are you human?" -- TODO: throw APE error instead of 'error'

         if asReply then
            return $ Left Nothing
          else do
            e <- newPaste Nothing   -- TODO: no user lookup function/support yet
                          filetype desc hidden content
            return $
              case e of
                    Left err  -> Left $ Just err
                    Right pId -> Right pId

  case r of
       Left (Just (APE_AlreadyExists Paste{pasteId})) -> do -- TODO: replace with proper ID
         let url = "/" ++ pasteId ++ "/"
         ResponseCode  .= (seeOther url :: Response -> ServerPart Response)
         PlainResponse .= toResponse $ "Paste already exists at: http://npaste.de" ++ url ++ "\n"
       Left err -> do
         HtmlFrame     .= mainFrame
         HtmlContext   .= nullContext { css = CSS ["index.css"] }
         HtmlBody      .= indexHtml pdata err
       Right pId -> do
         let url = "/" ++ pId ++ "/"
         ResponseCode  .= (seeOther url :: Response -> ServerPart Response)
         PlainResponse .= toResponse $ "New paste added: http://npaste.de" ++ url ++ "\n"

 where
  cutTrailingSpaces :: String -> String
  cutTrailingSpaces = unlines . map (reverse . dropWhile isSpace . reverse) . lines

  lookupLanguage :: String -> Maybe String
  lookupLanguage t =
    -- pick one of the following:
    listToMaybe $ [ l | l <- languages, map toLower l == map toLower t ]
               ++ languagesByExtension t
               ++ languagesByFilename t

--------------------------------------------------------------------------------
-- Post data

getIndexPostData :: NPaste IndexPostData
getIndexPostData = do
  decodeBody (defaultBodyPolicy "/tmp/npaste.de/" 1000000 1000000 1000000)
  fmap IndexPostData $ body lookPairs

nullPostData :: IndexPostData
nullPostData = IndexPostData []
