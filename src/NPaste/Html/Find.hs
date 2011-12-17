{-# LANGUAGE OverloadedStrings #-}

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
    listPastes pastes

findHtmlNothingFound :: Html
findHtmlNothingFound = do
  H.h1 ! A.class_ "error" $ "No search results."

--------------------------------------------------------------------------------
-- ** Tags

tagHtml :: String -> [Paste] -> Html
tagHtml tag pastes = do
  tagSearchForm $ Just tag
  findHtml ("All pastes for #" ++ tag) pastes

tagSearchHtml :: Html
tagSearchHtml = tagSearchForm Nothing

tagSearchForm :: Maybe String -> Html
tagSearchForm mtag = do
  H.form ! A.method "post" ! A.action "/t" $ do
    H.p "Search for tag: "
    case mtag of
         Just tag -> H.input ! A.type_ "text" ! A.name "tag" ! A.value (H.toValue tag)
         Nothing  -> H.input ! A.type_ "text" ! A.name "tag"
    H.input ! A.type_ "submit"
