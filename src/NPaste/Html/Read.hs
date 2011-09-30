{-# LANGUAGE OverloadedStrings, ViewPatterns, NamedFieldPuns #-}
{-# OPTIONS -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

module NPaste.Html.Read where

import Data.ByteString.Char8 (ByteString, unpack)
import Data.List                    as L
import Data.Time
import System.Locale
import Text.Blaze.Html5             as H
import Text.Blaze.Html5.Attributes  as A

import Text.Highlighting.Kate

import NPaste.Types

readHtml :: Maybe PostInfo -> Maybe ByteString -> Html
readHtml (Just p@PostInfo{ p_type = Just lang}) (Just cont) = do
  infoHtml p
  let cont' = unpack cont
  case highlightAs lang cont' of
       Right source -> formatCode source
       Left  err    -> do
         H.p ! A.class_ "warning" $ toHtml err
         H.pre $ toHtml cont'

readHtml (Just p) (Just cont) = do
  infoHtml p
  H.pre $ toHtml (unpack cont)

readHtml mp _ = do
  case mp of
       Just p -> infoHtml p
       _      -> return ()
  H.h3 ! A.class_ "error" $ "Paste not found."

-- | Turn source lines into HTML
formatCode :: [SourceLine]      -- ^ Source lines to format
           -> Html
formatCode source = H.div ! A.class_ "formatedCode" $ do
  H.div ! A.class_ "lineNumbers" $ H.pre $
    sequence_ . intersperse br . for [1..length source] $ \(show->n) ->
      H.a ! A.href (toValue $ "#line" ++ n) ! A.name (toValue $ "line" ++ n) $ toHtml n
  H.div ! A.class_ "sourceCode" $
    H.pre $ sequence_ . intersperse br $ L.map sourceLineToHtml source
 where
  for = flip L.map

sourceLineToHtml :: SourceLine -> Html
sourceLineToHtml []             = return ()
sourceLineToHtml (([],txt):r)   = toHtml txt >> sourceLineToHtml r
sourceLineToHtml ((labs,txt):r) = do
  let atts = drop 1 labs -- no idea why, lol
  if null atts
     then toHtml txt
     else H.span ! A.class_ (toValue $ intercalate " " atts) $ toHtml txt
  sourceLineToHtml r


-- | Show a nice header with all kind of informations about our paste
infoHtml :: PostInfo -> Html
infoHtml PostInfo{ p_id, p_date, p_description, p_type } =
  H.div ! A.class_ "postInfo" $ do
    H.p ! A.class_ "timestamp" $
      toHtml $ formatTime defaultTimeLocale "%H:%M - %a %Y.%m.%d" p_date
    H.p ! A.class_ "language" $
      case p_type of
           Nothing -> "Plain text"
           Just t  -> toHtml t
    H.p $ do
      "Paste: "
      H.a ! A.href (toValue $ "/" ++ p_id) $
        toHtml $ "/" ++ p_id ++ "/"
    H.p ! A.class_ "desc" $
      case p_description of
           Just d  -> toHtml d
           Nothing -> "No description."

-- | Show recent posts
recentHtml :: [(PostInfo, Maybe ByteString)] -> Html
recentHtml []               =
  return ()
recentHtml ((p, Just c):r)  = do
  infoHtml p
  let cont = unlines . take 20 . lines $ unpack c
      lang = p_type p
  case lang of
       Nothing -> H.pre $ toHtml cont
       Just l  ->
         case highlightAs l cont of
             Right source -> formatCode source
             Left  err    -> do
               H.p ! A.class_ "warning" $ toHtml err
               H.pre $ toHtml cont
  recentHtml r
recentHtml ((p, Nothing):r) = do
  infoHtml p
  H.h3 ! A.class_ "error" $ "Content not found/invalid."
  recentHtml r
