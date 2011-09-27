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

    H.div ! A.id "settings" $ do

      let desc    = getValue pdata "desc"
          defdesc = "Enter description..."
      if null desc || desc == defdesc then
        H.input !  A.id "desc"
                ! A.type_ "text"
                ! A.name "desc"
                ! A.value (toValue defdesc)
                ! A.class_ "new-desc"
       else
        H.input ! A.id "desc" ! A.type_ "text" ! A.name "desc" ! A.value (toValue desc)

      H.select ! A.id "lang" ! A.name "lang" $
        forM_ languages $ \l ->
          if l == getValue pdata "lang" then
            H.option ! A.selected "selected" ! A.value (toValue l) $ toHtml l
           else
            H.option                         ! A.value (toValue l) $ toHtml l
      H.input ! A.id "submit" ! A.type_ "submit" ! A.name "submit" ! A.value "OK"

    H.textarea ! A.id "content" ! A.name "content" $
      toHtml $ getValue pdata "content"
