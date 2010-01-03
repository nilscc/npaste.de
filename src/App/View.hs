{-# OPTIONS_GHC -F -pgmFtrhsx #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeFamilies,
    MultiParamTypeClasses, UndecidableInstances, NoMonomorphismRestriction
    #-}

module App.View
    ( Css (..)
    , HtmlBody (..)
    , HtmlOptions (..)

    , xmlResponse
    , renderBody
    , htmlToXml
    , pandocToXml

    , xmlMetaData

    , (<-<)

    ) where


import Control.Monad.Trans (MonadIO (..))
import Data.List (find)

import HSP
import Happstack.Server
import Happstack.Server.HSP.HTML (webHSP')

import Text.XHtml
import Text.Pandoc               (Pandoc (..), defaultWriterOptions, writeHtml)

--------------------------------------------------------------------------------
-- Data definitions
--------------------------------------------------------------------------------

-- | Css data
data Css = CssString { cssString :: String }
         | CssFile   { cssFile   :: FilePath }

-- | HTML Body data
data HtmlBody = HtmlBody [HtmlOptions] [HSP XML]

-- | HTML Options
data HtmlOptions = WithCss Css
                 | WithTitle String
                 -- | WithJavascript JavaScript
                 | WithLogo String String String


--------------------------------------------------------------------------------
-- Pure stuff
--------------------------------------------------------------------------------

-- | Get all Css elements out of a list of HtmlOptions
getCssFromOptions :: [HtmlOptions] -> [Css]
getCssFromOptions = foldr step []
  where step (WithCss css) r = css : r
        step _ r = r

-- Helper
unpack (Just (WithTitle str)) = Just $ <title><% str %></title>

unpack (Just (WithLogo left right desc)) = Just $
    <div id="logo">
        <p id="left"><% left %></p><p id="center">::</p><p id="right"><% right %></p>
        <p id="info"><% desc %></p>
    </div>

unpack _                      = Nothing

-- | Get the first Title element of a list of HtmlOptions
getTitleFromOptions :: [HtmlOptions] -> Maybe (HSP XML)
getTitleFromOptions = unpack . find isTitle
  where isTitle (WithTitle _) = True
        isTitle _             = False

-- | Get the first Logo element of a list of HtmlOptions
getLogoFromOptions :: [HtmlOptions] -> Maybe (HSP XML)
getLogoFromOptions = unpack . find isLogo
  where isLogo (WithLogo _ _ _) = True
        isLogo _                = False

-- | Html -> HSP XML convertion
-- Use...
--
-- > import qualified HSX.XMLGenerator   as HSX
-- > instance (XMLGenerator m, EmbedAsChild m XML, HSX.XML m ~ XML) => (EmbedAsChild m ...) where ...
-- 
-- ...for your EmbedAsChild instance definitions.
htmlToXml :: (Monad m, HTML a)
          => a -> XMLGenT m XML
htmlToXml = XMLGenT . return . cdata . showHtmlFragment

-- | Pandoc -> HSP XML convertion
pandocToXml :: (Monad m)
            => Pandoc -> XMLGenT m XML
pandocToXml = htmlToXml . writeHtml defaultWriterOptions


-- | Combine two HtmlBody elements
(<-<) :: HtmlBody -> HtmlBody -> HtmlBody
(HtmlBody o1 e1) <-< (HtmlBody o2 e2) = HtmlBody (o1 ++ o2) (e1 ++ e2)

--------------------------------------------------------------------------------
-- XML instances
--------------------------------------------------------------------------------

-- | XML Generator for Css
instance (XMLGenerator m) => (EmbedAsChild m Css) where
    asChild (CssString str) =
        <%
            <style type="text/css">
                <% str %>
            </style>
        %>
    asChild (CssFile file) =
        <%
            <link rel="stylesheet" type="text/css" href=(file) />
        %>


--------------------------------------------------------------------------------
-- Render HTML body
--------------------------------------------------------------------------------

renderBody :: HtmlBody -> HSP XML
renderBody (HtmlBody options xml) =
    <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="de" lang="de">
        <head>
            <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
            <% title %>
            <% css %>
        </head>
        <body>
            <% logo %>
            <% xml %>
        </body>
    </html>
  where css     = getCssFromOptions   options
        title   = getTitleFromOptions options
        logo    = getLogoFromOptions  options


--------------------------------------------------------------------------------
-- ServerPartT element
--------------------------------------------------------------------------------

xmlResponse :: (MonadIO m)
            => HtmlBody -> ServerPartT m Response
xmlResponse = webHSP' (Just xmlMetaData) . renderBody

xmlMetaData = XMLMetaData { doctype = (True, "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
                          , contentType = "text/html;charset=utf-8"
                          , preferredRenderer = renderAsHTML
                          }
