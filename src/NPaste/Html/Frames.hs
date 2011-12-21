{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-unused-do-bind #-}

module NPaste.Html.Frames
  ( mainFrame
  , compactFrame
  , nullContext
  ) where

import Data.Maybe
import Data.List
import Text.Blaze (toHtml, toValue, (!))
import qualified Text.Blaze.Html5             as H
import qualified Text.Blaze.Html5.Attributes  as A

import NPaste.Html.Menu
import NPaste.Types

nullContext :: HtmlContext
nullContext = HtmlContext
  { title = Title Nothing
  , menu = nullMenu
  , script = Script []
  , css = CSS []
  }

htmlHeader :: HtmlContext -> Html
htmlHeader htmlcontext =
  H.head $ do
    H.title . toHtml $
      maybe "npaste.de" ("npaste.de - " ++) (unTitle $ title htmlcontext)

    -- load javascript
    unless (null . unScript $ script htmlcontext) $ do
      let scripts = ["jquery-1.6.2.min.js"] ++ unScript (script htmlcontext)
      forM_ scripts $ \s ->
        H.script ! A.type_ "text/javascript" ! A.src (toValue $ "/s/js/" ++ s) $ return ()

    -- load css
    let cssFiles = unCSS (css htmlcontext) ++ ["fonts.css"]
    forM_ cssFiles $ \c ->
      H.link ! A.type_ "text/css" ! A.href (toValue $ "/s/css/" ++ c) ! A.rel "stylesheet"

--------------------------------------------------------------------------------
-- Main frame

mainFrame :: HtmlContext
          -> HtmlBody
          -> Html
mainFrame htmlcontext htmlbody = H.docTypeHtml $ do
  htmlHeader htmlcontext{ css = CSS $ ["main.css"] ++ unCSS (css htmlcontext) }

  H.body $ do
    H.header $ mainHeader
    H.menu   $ mainMenu (menu htmlcontext)
    H.section ! A.id "main" $ unHtmlBody htmlbody

-- | Header
mainHeader :: Html
mainHeader = do
  H.p ! A.id "left" $ do
    H.a ! A.id "n3" ! A.href "/a" $ do
      "n"
      H.sup "3"
    "paste.de"
  H.p ! A.id "center" $
    "::"
  H.p ! A.id "right" $
    "IO String"
  H.p ! A.id "info" $
    "a haskell happstack pastebin"

-- | Menu
mainMenu :: Menu -> Html
mainMenu Menu{ activeMenuSection = ActiveMenu active
             , menuStructure     = MenuStructure mstr } =
  sequence_ $ do -- list monad
    s <- mstr
    return $
      if (isCurrentMenu active s) then
         H.li ! A.class_ "active" $ do
           H.a ! A.href (menuLink s) $ menuTitle s
           -- build submenu if available
           let subm = subMenu active
           unless (null subm) $ H.ul ! A.class_ "submenu" $
             mapM_ H.li subm
       else
         H.li $ H.a ! A.href (menuLink s) $ menuTitle s


--------------------------------------------------------------------------------
-- Compact frame

compactFrame :: Html        -- ^ header content
             -> HtmlContext -- ^ HTML frame
             -> HtmlBody    -- ^ inner HTML body
             -> Html
compactFrame compH htmlcontext htmlbody = H.docTypeHtml $ do
  htmlHeader htmlcontext{ css = CSS $ unCSS (css htmlcontext) ++ ["compact.css"] }
  H.body $ do
    H.header                $ compH
    H.section ! A.id "main" $ unHtmlBody htmlbody
