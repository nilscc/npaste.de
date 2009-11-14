{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses,
    UndecidableInstances, FlexibleContexts, TypeFamilies,
    NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Paste.View (showPaste) where

import Happstack.Server
import Happstack.State
import Happstack.Server.HSP.HTML (webHSP)

import HSP
import qualified HSX.XMLGenerator as HSX
import Text.Highlighting.Kate
import qualified Text.XHtml as X
import Text.XHtml
    ( (+++)
    , (<<)
    , (!)
    )

import Control.Monad (msum, mzero, liftM)
import Control.Monad.Trans (liftIO)
import Data.Char  (toLower)
import Data.Maybe (fromMaybe)

-- import Paste.View
import Paste.State
import Users.State
    ( Validate (..)
    , UserReply (..)
    , Login (..)
    , Password (..)
    )

-- | Show paste handler
showPaste :: String -> ServerPartT IO Response
showPaste id = do
    mPaste <- query $ GetPasteById (ID id)
    case mPaste of
         Nothing -> notFound . toResponse $ "Paste not found with ID \"" ++ id ++ "\""
         Just p  -> do
             msum [ path $ showWithSyntax p
                  , trailingSlash >> showWithSyntax p (fromMaybe "" $ filetype p)
                  , showPlain p
                  ]

-- | Simple method to get the content string of a PasteEntry
getContent :: PasteEntry -> IO String
getContent p = case content p of
                    Plain text -> return $ text
                    File file  -> readFile file

-- | Show plain text
showPlain :: PasteEntry -> ServerPartT IO Response
showPlain p = liftIO (getContent p) >>= ok . setHeader "Content-Type" "text/plain" . toResponse

-- | Syntax highlighting
showWithSyntax :: PasteEntry -> String -> ServerPartT IO Response
showWithSyntax p ext
    | ext' `elem` tinyIds = do
        url <- liftIO $ getContent p
        seeOther (head $ lines url) $ toResponse url

    | otherwise = do
    cont <- liftIO $ getContent p

    let paste = case highlightAs ext cont of
                     Left _   -> PlainText   { lang'       = filetype p
                                             , plainText   = cont
                                             }
                     Right sl -> Highlighted { lang        = ext
                                             , plainText   = cont
                                             , highlighted = formatAsXHtml [] ext sl
                                             }

    webHSP $ pasteBody (CssString defaultHighlightingCss) (unId $ pId p) paste

  where ext' = map toLower ext


--------------------------------------------------------------------------------
-- VIEW part
--------------------------------------------------------------------------------

-- | Main paste body
pasteBody :: Css -> String -> PasteContent -> HSP XML
pasteBody css id content =
    <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="de" lang="de">
        <head>
            <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
            <title>npaste.de - Paste #<% id %></title>
            <% css %>
            <link href="/static/style.css" type="text/css" rel="stylesheet" />
        </head>
        <body>
            <div id="topmenu">
                <p id="view_plain"><a href=("/" ++ id)>View plain</a></p>
                <p id="available_languages">Available languages:
                <select size="1" id="languages" 
                  onChange="menu = document.getElementById('languages'); window.location = menu.options[menu.selectedIndex].text">
                    <%
                        map option (("Current: " ++ (language content)) : languages)
                    %>
                </select>
                </p>
            </div>
            <% content %>
        </body>
    </html>
  where option l = <option><% l %></option>
        language (Highlighted l _ _) = language' l
        language (PlainText l _)     = language' $ fromMaybe "" l
        language' s | (not . null $ languagesByExtension s) = head $ languagesByExtension s
                    | s `elem` languages                    = s
                    | otherwise                             = "Plain text"




-- | <pre> tag for source code (with maybe language extension)
sourcePre Nothing code = <pre class="sourceCode"><% code %></pre>
sourcePre (Just ext) code = <pre class="sourceCode"><% code %></pre>

-- | Paste data
data PasteContent = PlainText   { lang'       :: Maybe String
                                , plainText   :: String
                                }
                  | Highlighted { lang        :: String
                                , plainText   :: String
                                , highlighted :: X.Html
                                }

-- | Css data
data Css = CssString { cssString :: String }
         | CssFile   { cssFile   :: FilePath }

-- | XML generator for PasteContent
instance (XMLGenerator m, EmbedAsChild m XML, HSX.XML m ~ XML) => (EmbedAsChild m PasteContent) where
    asChild content =
        <%
          <table class="sourceCode">
            <tr>
              <td class="lineNumbers" title="Click to toggle line numbers"
                  onClick="with (this.firstChild.style) { display = (display == '') ? 'none' : '' }"
                  >
                <pre><%
                    let -- clickable n = <a hef=("#n" ++ n) class="no" name=("n" ++ n) id=("n" ++ n)><% n %></a>
                    in unlines . map (fst) $ zip (map show [1..]) (lines text)
                %></pre>
              </td>
              <td class="sourceCode">
                <% case content of
                        PlainText _ _        -> <pre><% text %></pre>
                        Highlighted _ _ html -> toXML html
                %>
              </td>
            </tr>
          </table>
        %>
      where text = case content of
                        PlainText _ str     -> str
                        Highlighted _ str _ -> str

-- WHY?!
-- instance (XMLGenerator m) => (EmbedAsChild m X.Html) where
    -- asChild html = pcdata . X.showHtmlFragment $ html

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
            <link rel="stylesheet" type="text/css" href="<% file %>" />
        %>

toXML :: (EmbedAsChild m XML) => X.Html -> XMLGenT m XML 
toXML = XMLGenT . return . cdata . X.showHtmlFragment
