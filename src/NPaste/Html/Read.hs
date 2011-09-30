{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module NPaste.Html.Read where

import Data.ByteString.Char8 (ByteString, unpack)
import Data.List                    as L
import Text.Blaze.Html5             as H
import Text.Blaze.Html5.Attributes  as A

import Text.Highlighting.Kate

import NPaste.Types

readHtml :: Maybe PostInfo -> Maybe ByteString -> Html
readHtml (Just PostInfo{ p_type = Just lang}) (Just cont) = do
  let cont' = unpack cont
  case highlightAs lang cont' of
       Right source -> formatCode source
       Left  err    -> do
         H.p ! A.class_ "warning" $ toHtml err
         H.pre $ toHtml cont'

readHtml _ (Just cont) = do
  H.pre $ toHtml (unpack cont)

readHtml _ _ =
  H.h3 ! A.class_ "error" $ "Paste not found."

formatCode :: [SourceLine]      -- ^ Source lines to format
           -- -> String            -- ^ Language
           -> Html
formatCode source = H.div ! A.class_ "formatedCode" $ do
  H.div ! A.class_ "lineNumbers" $ H.pre $
    sequence_ . intersperse br . for [1..length source] $ \(show->n) ->
      H.a ! A.href (toValue $ "#line" ++ n) ! A.name (toValue $ "#line" ++ n) $ toHtml n
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
