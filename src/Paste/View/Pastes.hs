{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses,
    UndecidableInstances, FlexibleContexts, TypeFamilies,
    NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Paste.View.Pastes (showPaste) where

import Happstack.Server
import Happstack.State
import Happstack.Server.HSP.HTML    (webHSP)

import HSP
import Text.Highlighting.Kate
import Text.XHtml                   ((+++), (<<), (!))
import qualified HSX.XMLGenerator   as HSX
import qualified Text.XHtml         as X

import Control.Monad                (msum, mzero, liftM)
import Control.Monad.Trans          (liftIO)
import Data.Char                    (toLower)
import Data.Maybe                   (fromMaybe)
import Data.List                    (find)

import App.View                     (Css (..), htmlToXml)
import Paste.State
import Users.State                  (Validate (..), UserReply (..))





--------------------------------------------------------------------------------
-- Get data, handle events
--------------------------------------------------------------------------------

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
        seeOther (HSP.escape . head $ lines url) $ toResponse url

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

        webHSP $ pasteBody (CssString defaultHighlightingCss) (unId $ pId p) paste (description p)

      where ext' = map toLower ext







--------------------------------------------------------------------------------
-- Data stuff
--------------------------------------------------------------------------------

-- | Paste data
data PasteContent = PlainText   { lang'       :: Maybe String
                                , plainText   :: String
                                }
                  | Highlighted { lang        :: String
                                , plainText   :: String
                                , highlighted :: X.Html
                                }


--------------------------------------------------------------------------------
-- VIEW part
--------------------------------------------------------------------------------

type Description = Maybe String

-- | Main paste body
pasteBody :: Css -> String -> PasteContent -> Description -> HSP XML
pasteBody css id content desc =
    <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="de" lang="de">
        <head>
            <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
            <title>npaste.de - Paste #<% id ++ (maybe "" (": " ++) desc)%></title>
            <% css %>
            <link href="/static/style.css" type="text/css" rel="stylesheet" />
        </head>
        <body>
            <div id="topmenu">
                <p id="description"><% maybe "No description." ("Description: " ++) desc %></p>
                <div id="right">
                    <p id="view_plain"><a href=("/" ++ id)>View plain</a></p>
                    <p id="available_languages">Available languages:
                    <select size="1" id="languages" 
                      onChange="menu = document.getElementById('languages'); window.location = menu.options[menu.selectedIndex].text">
                        <%
                            map langOptions ("Text" : languages)
                        %>
                    </select>
                    </p>
                </div>
                <div id="clear"></div>
            </div>
            <% content %>
        </body>
    </html>
  where langOptions l | l == (language content) = <option selected="selected"><% l ++ " (current)" %></option>
                      | otherwise               = <option><% l %></option>
        language (Highlighted l _ _) = language' l
        language (PlainText l _)     = language' $ fromMaybe "" l
        language' s = case find (\l -> (map toLower s) == (map toLower l)) languages of
                           Just l -> l
                           _ | (not . null $ languagesByExtension s) -> head $ languagesByExtension s
                             | otherwise                             -> "Text"




-- | <pre> tag for source code (with maybe language extension)
sourcePre Nothing code = <pre class="sourceCode"><% code %></pre>
sourcePre (Just ext) code = <pre class="sourceCode"><% code %></pre>



--------------------------------------------------------------------------------
-- XML instances
--------------------------------------------------------------------------------

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
                    let clickable n = <a href=("#n" ++ n) class="no" name=("n" ++ n) id=("n" ++ n)><% n ++ "\n" %></a>
                    in map clickable . map (fst) $ zip (map show [1..]) (lines text)
                %></pre>
              </td>
              <td class="sourceCode">
                <% case content of
                        PlainText   _ _      -> <pre><% "\n" ++ text %></pre>
                        Highlighted _ _ html -> htmlToXml html
                %>
              </td>
            </tr>
          </table>
        %>
      where text = case content of
                        PlainText   _ str   -> str
                        Highlighted _ str _ -> str
