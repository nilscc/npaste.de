{-# LANGUAGE OverloadedStrings #-}

module NPaste.Html.Read where

import Data.ByteString.Char8 (ByteString, unpack)
import Text.Blaze.Html5             as H
import Text.Blaze.Html5.Attributes  as A

import NPaste.Types

readHtml :: Maybe PostInfo -> Maybe ByteString -> Html
readHtml (Just _pinfo) (Just cont) = do
  H.pre $ toHtml $ unpack cont

readHtml _ _ =
  H.h1 ! A.class_ "error" $ "Paste not found."
