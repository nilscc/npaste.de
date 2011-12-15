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

tagHtml :: String -> [Paste] -> Html
tagHtml tag pastes =
  findHtml ("All pastes for #" ++ tag) pastes
