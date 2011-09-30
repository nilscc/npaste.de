{-# LANGUAGE NamedFieldPuns #-}

module NPaste.Routes.Index where

import Happstack.Server
import Data.ByteString.Char8 (pack)
import Data.Char

import NPaste.Database
import NPaste.Html
import NPaste.Types


indexR :: ServerPart Response
indexR = do
  pdata <- msum
    [ methodM POST >> getIndexPostData
    , return nullPostData ]

  r <- if pdata == nullPostData then
         return $ Left Nothing
        else do
         let filetype  = case getValue pdata "lang" of
                              t | t == "Plain text" -> Nothing
                                | null t            -> Nothing
                                | otherwise         -> Just t
             desc      = case getValue pdata "desc" of
                              d | null d    -> Nothing
                                | otherwise -> Just d
             hidden    = getValue pdata "hidden" == "on"
             idSetting = IdRandom
             content   = pack $ cutTrailingSpaces $ getValue pdata "content"
         e <- newPost Nothing   -- no user lookup function yet TODO
                      filetype desc hidden idSetting content
         return $
           case e of
                Left err  -> Left $ Just err
                Right pId -> Right pId

  case r of
       Left (Just (APE_AlreadyExists mu pId)) ->
         let url = case mu of
                        Just User{u_name} -> "/u/" ++ u_name ++ "/" ++ pId
                        Nothing           -> "/r/" ++ pId
          in seeOther url (toResponse $ "Paste already exists at: http://npaste.de" ++ url)
       Left err -> 
         return . toResponse . mainFrame $ nullBody
           { css    = ["index.css"]
           -- , script = ["index.js"]
           , html   = indexHtml pdata err
           }
       Right pId -> do
         let url = case pId of
                        ID pId'                     -> "/r/" ++ pId'
                        PrivateID User{u_name} pId' -> "/u/" ++ u_name ++ "/" ++ pId'
         seeOther url (toResponse $ "New paste added: http://npaste.de" ++ url)
 where
  cutTrailingSpaces :: String -> String
  cutTrailingSpaces = unlines . map (reverse . dropWhile isSpace . reverse) . lines

--------------------------------------------------------------------------------
-- Post data

getIndexPostData :: ServerPart IndexPostData
getIndexPostData = do
  decodeBody (defaultBodyPolicy "/tmp/npaste.de/" 4096 4096 4096)
  fmap IndexPostData $ body lookPairs

nullPostData :: IndexPostData
nullPostData = IndexPostData []
