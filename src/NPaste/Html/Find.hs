{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

module NPaste.Html.Find where

import Text.Blaze.Html5            as H
import Text.Blaze.Html5.Attributes as A

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

viewHtml :: Maybe String -> [Paste] -> Html
viewHtml mtag pastes = do
  H.h1 $ toHtml $
    case mtag of
         Just t  -> "Pastes filtered by #" ++ t
         Nothing -> "Most recent pastes"
  tagSearchForm mtag
  H.div ! A.id "paste_list" $
    if null pastes then
      H.p ! A.class_ "error" $ "No pastes found."
     else
      listPastes pastes (Just 20)


--------------------------------------------------------------------------------
-- ** Tags

tagSearchForm :: Maybe String -> Html
tagSearchForm mtag = do
  H.form ! A.method "post" ! A.action "/v/t" ! A.id "tag_search_form" $ do
    H.p $ do
      "Filter pastes ("
      H.a ! A.href "/a#filter" $ "help"
      "):"
    case mtag of
         Just tag -> H.input ! A.type_ "text" ! A.name "tag" ! A.value (H.toValue tag)
         Nothing  -> H.input ! A.type_ "text" ! A.name "tag"
    H.input ! A.type_ "submit"
