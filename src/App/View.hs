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

    ) where


import Control.Monad.Trans (MonadIO (..))
import Data.List (find)

import HSP
import Happstack.Server
import Happstack.Server.HSP.HTML (webHSP)

import Text.XHtml

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


--------------------------------------------------------------------------------
-- Pure stuff
--------------------------------------------------------------------------------

-- | Get all Css elements out of a list of HtmlOptions
getCssFromOptions :: [HtmlOptions] -> [Css]
getCssFromOptions = foldr step []
  where step (WithCss css) r = css : r
        step _ r = r

-- | Get the first Title element of a list of HtmlOptions
getTitleFromOptions :: [HtmlOptions] -> Maybe (HSP XML)
getTitleFromOptions = unpack . find isTitle
  where isTitle (WithTitle _) = True
        isTitle _             = False
        unpack (Just (WithTitle str)) = Just $ <title><% str %></title>
        unpack _                      = Nothing

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
            <% xml %>
        </body>
    </html>
  where css     = getCssFromOptions   options
        title   = getTitleFromOptions options


--------------------------------------------------------------------------------
-- ServerPartT element
--------------------------------------------------------------------------------

xmlResponse :: (MonadIO m)
            => HtmlBody -> ServerPartT m Response
xmlResponse = webHSP . renderBody
