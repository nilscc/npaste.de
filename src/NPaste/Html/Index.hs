{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

module NPaste.Html.Index where

import Text.Blaze.Html5             as H
import Text.Blaze.Html5.Attributes  as A
import Text.Highlighting.Kate (languages)

import NPaste.Types

indexHtml :: IndexPostData -> Maybe AddPostError -> Html
indexHtml pdata err = do
  H.h1 "New paste"

  -- show error messages
  case err of 
       Just APE_NoContent ->
         H.p ! A.class_ "error" $ "No content given."
       Just APE_UserRequired ->
         H.p ! A.class_ "error" $ do
           "Registered users only, "
           H.a ! A.href "/register" $ "register now!"
       Just (APE_InvalidCustomId _) ->
         H.p ! A.class_ "error" $ "You custom ID is invalid."
       Just (APE_AlreadyExists _ _) ->
         H.p ! A.class_ "error" $ "Your paste already exists."
       Just (APE_Other s) ->
         H.p ! A.class_ "error" $ toHtml $ "An error occured: " ++ s
       Just (APE_DescTooLong) ->
         H.p ! A.class_ "error" $ "Your description is too long (250 char max)."
       Nothing   -> return ()

  H.form ! A.method "post" ! A.action "/" $ do

    H.div ! A.class_ "settings" $ do

      H.p "Description:"
      let desc = getValue pdata "desc"
      H.input ! A.id "desc" ! A.type_ "text" ! A.name "desc" ! A.value (toValue desc)

    H.textarea ! A.id "content" ! A.name "content" $
      toHtml $ getValue pdata "content"

    H.ul ! A.class_ "settings" $ do

      H.li $ do
        H.p "Language:"
        H.select ! A.id "lang" ! A.name "lang" $
          forM_ ("Plain text" : languages) $ \l ->
            if l == getValue pdata "lang" then
              H.option ! A.selected "selected" ! A.value (toValue l) $ toHtml l
             else
              H.option                         ! A.value (toValue l) $ toHtml l

      H.li $ do
        H.p "Hide from recent pastes:"
        if getValue pdata "hidden" == "on" then
          H.input ! A.type_ "checkbox" ! A.id "hidden" ! A.name "hidden"
                  ! A.checked "checked"
         else
          H.input ! A.type_ "checkbox" ! A.id "hidden" ! A.name "hidden"

    -- spam checker, this shouldn't be visible at all. not sure if it's really
    -- a good method to do that, but it's better than nothing :)
    H.input ! A.id "email" ! A.type_ "text" ! A.name "email"

    H.input ! A.id "submit" ! A.type_ "submit" ! A.name "submit" ! A.value "Add paste"
