{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

module NPaste.Html.Find where

import Text.Blaze.Html5 ((!), toHtml)
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A

import Text.ParserCombinators.Parsec.Error

import NPaste.Html.Read
import NPaste.Types

findHtml :: String -> [Paste] -> Html
findHtml t pastes = do
  H.h1 $ toHtml t
  if null pastes then
    H.p ! A.class_ "error" $ "No pastes found."
   else
    listPastes pastes (Just 20)

findHtmlNothingFound :: Html
findHtmlNothingFound = do
  H.h1 ! A.class_ "error" $ "No search results."


--------------------------------------------------------------------------------
-- ** View pastes

viewHtml :: Maybe String -> Either ParseError [Paste] -> Html
viewHtml mf epastes = do
  H.h1 $ do
    "Most recent pastes"
    maybe (return ()) (const " (filtered)") mf
  filterForm mf
  H.div ! A.id "paste_list" $
    case epastes of
         Right [] -> H.p ! A.class_ "error" $ "No pastes found."
         Left err -> H.p ! A.class_ "error" $ toHtml $
           showErrorMessages "or" "?" "instead of"
             "Invalid filter request: Unexpected" "end of input"
             (errorMessages err)
         Right ps -> listPastes ps (Just 20)

filterForm :: Maybe String -> Html
filterForm mf = do
  H.form ! A.method "post" ! A.action "/v" ! A.id "filter_form" $ do
    H.p $ do
      "Filter pastes ("
      H.a ! A.href "/a#filter" $ "help"
      "):"
    case mf of
         Just  f -> H.input ! A.type_ "text" ! A.name "filter" ! A.value (H.toValue f)
         Nothing -> H.input ! A.type_ "text" ! A.name "filter"
    H.input ! A.type_ "submit"
