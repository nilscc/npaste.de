{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

module NPaste.Html.MainFrame where

import Text.Blaze (toHtml, toValue, (!))
import qualified Text.Blaze.Html5             as H
import qualified Text.Blaze.Html5.Attributes  as A

import NPaste.Types

nullBody :: HtmlBody
nullBody = HtmlBody
  { title = Nothing
  , section = M_AddNewPaste
  , user = Nothing
  , script = []
  , css = []
  , html = return ()
  }

--------------------------------------------------------------------------------
-- Main HTML

mainFrame :: HtmlBody
          -> Html
mainFrame htmlbody = H.docTypeHtml $ do
  H.head $ do
    H.title . toHtml $
      maybe "npaste.de" ("npaste.de - " ++) (title htmlbody)

    -- load javascript
    unless (null $ script htmlbody) $ do
      let scripts = ["jquery-1.6.2.min.js"] ++ script htmlbody
      forM_ scripts $ \s ->
        H.script ! A.type_ "text/javascript" ! A.src (toValue $ "/js/" ++ s) $ return ()

    -- load css
    let cssFiles = css htmlbody ++ ["fonts.css"]
    forM_ cssFiles $ \c ->
      H.link ! A.type_ "text/css" ! A.href (toValue $ "/css/" ++ c) ! A.rel "stylesheet"

  H.body $ do
    H.header $ mainHeader
    H.menu   $ mainMenu (section htmlbody)
    H.section ! A.id "main" $
      html htmlbody


--------------------------------------------------------------------------------
-- Header

mainHeader :: Html
mainHeader = do
  H.p ! A.id "left" $ do
    H.a ! A.id "n3" ! A.href "/about" $ do
      "n"
      H.sup "3"
    "paste.de"
  H.p ! A.id "center" $
    "::"
  H.p ! A.id "right" $
    "IO String"
  H.p ! A.id "info" $
    "a haskell happstack pastebin"
  

--------------------------------------------------------------------------------
-- Menu

mainMenu :: MenuSection -> Html
mainMenu active = sequence_ $ do
  -- list monad
  s <- [ M_AddNewPaste, M_Read, M_Settings]
  return $ H.li $
    if (active == s) then
      H.a ! A.href (sectionToUrl s) ! A.class_ "menu-item active" $ (sectionToTitle s)
     else
      H.a ! A.href (sectionToUrl s) $ (sectionToTitle s)

sectionToUrl :: MenuSection -> AttributeValue
sectionToUrl s = case s of
  M_AddNewPaste -> "/"
  M_Read        -> "/r"
  M_Settings    -> "/settings"

sectionToTitle :: MenuSection -> Html
sectionToTitle s = case s of
  M_AddNewPaste -> "New paste"
  M_Read        -> "Read recent pastes"
  M_Settings    -> "My settings"
