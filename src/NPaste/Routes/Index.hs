{-# LANGUAGE NamedFieldPuns #-}

module NPaste.Routes.Index where

import Happstack.Server
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString as B
import Data.Char
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Text.Encoding.Error
import Data.Time

import NPaste.Database
import NPaste.Html
import NPaste.Types
import NPaste.State
import NPaste.Utils


indexR :: NPaste ()
indexR = do

  ActiveMenu .= Just M_Index

  pdata <- choice
    [ methodM POST >> getPostData
    , return nullPostData ]

  r <- if pdata == nullPostData then
         return $ Left Nothing
        else do
         let filetype  = findLang . unpack $ getValue pdata "lang"
             desc      = case unpack $ getValue pdata "desc" of
                              d | null d    -> Nothing
                                | otherwise -> Just d
             hidden    = getValue pdata "hidden" == pack "on"
             content   = encodeUtf8 . cutTrailingSpaces . decodeUtf8With ignore $ getValue pdata "content"
             spam      = not . B.null $ getValue pdata "email" -- should always be null!
             asReply   = not . B.null $ getValue pdata "asreply"

         when spam $ do
           -- TODO: pretty hacky :]
           t <- liftIO $ getCurrentTime
           liftIO $ appendFile "spam.log" $ show t
           error "Are you human?" -- TODO: throw APE error instead of 'error'

         if asReply then
            return $ Left Nothing
          else do
            mu <- getCurrentUser
            e <- addPaste mu filetype desc hidden content
            return $
              case e of
                    Left err  -> Left $ Just err
                    Right pId -> Right pId

  case r of
       Right pId -> do
         let url = "/" ++ pId ++ "/"
         rq <- askRq
         PlainResponse rq .= seeOther url . toResponse $
                             "New paste added: http://npaste.de" ++ url ++ "\n"
       Left (Just (APE_AlreadyExists Paste{pasteId})) -> do -- TODO: replace with proper ID
         let url = "/" ++ pasteId ++ "/"
         rq <- askRq
         PlainResponse rq .= seeOther url . toResponse $
                             "Paste already exists at: http://npaste.de" ++ url ++ "\n"
       Left err -> do
         mu <- getCurrentUser
         HtmlFrame .= mainFrame
         CSS       .= ["index.css"]
         HtmlBody  .= indexHtml mu pdata err

 where
  cutTrailingSpaces :: T.Text -> T.Text
  cutTrailingSpaces = T.unlines . map (T.reverse . T.dropWhile isSpace . T.reverse) . T.lines
