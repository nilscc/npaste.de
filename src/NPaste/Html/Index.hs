{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

module NPaste.Html.Index where

import Text.Blaze.Html5             as H
import Text.Blaze.Html5.Attributes  as A

import NPaste.Types


-- todo :)
languages :: [String]
languages =
  [ "C"
  , "Haskell"
  , "HTML"
  , "JavaScript"
  , "Python"
  ]


indexHtml :: IndexPostData -> Html
indexHtml pdata = do
  H.h1 "New post"
  H.form ! A.method "post" ! A.action "/" $ do
    H.input ! A.type_ "text" ! A.name "desc" ! A.value (pdata ? "desc")
    H.select ! A.name "lang" $
      forM_ languages $ \l ->
        if l == getValue pdata "lang" then
          H.option ! A.selected "selected" ! A.value (toValue l) $ toHtml l
         else
          H.option                         ! A.value (toValue l) $ toHtml l
    H.input ! A.type_ "submit" ! A.name "submit" ! A.value "OK"
