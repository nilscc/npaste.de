{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses,
    UndecidableInstances, FlexibleContexts, TypeFamilies,
    NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Paste.View.Pastes
    ( showPaste
    , parsedDesc
    ) where

import Happstack.Server
import Happstack.State
import Happstack.Server.HSP.HTML    (webHSP')

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
import qualified Paste.Parser.Description as PPD (parseDesc, DescVal(..))





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
    -- | ext' `elem` tinyIds = do
        -- url <- liftIO $ getContent p
        -- seeOther (HSP.escape . head $ lines url) $ toResponse url

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

        ids <- query $ GetAllIds
        show_all <- getDataQueryFn $ look "replies"
        webHSP' (Just xmlMetaData) $ pasteBody (CssString defaultHighlightingCss)
                                               (unId $ pId p)
                                               ids
                                               paste
                                               (description p)
                                               (show_all == Just "all")
                                               (responses p)

      where ext' = map toLower ext
            xmlMetaData = XMLMetaData { doctype = (True, "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">")
                                      , contentType = "text/html"
                                      , preferredRenderer = renderXML
                                      }







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
pasteBody :: Css            -- ^ css stuff
          -> String         -- ^ current ID
          -> [ID]           -- ^ all IDs
          -> PasteContent   -- ^ content
          -> Description    -- ^ description
          -> Bool           -- ^ show all replies?
          -> [ID]           -- ^ all replies
          -> HSP XML
pasteBody css id ids content desc show_all resp =
        <html xmlns="http://www.w3.org/1999/xhtml" xml:lang="de" lang="de">
            <head>
                <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
                <title>npaste.de - Paste #<% id ++ (maybe "" (const $ ": " ++ shortDesc) desc)%></title>
                <% css %>
                <link href="/static/css/paste.css" type="text/css" rel="stylesheet" />
            </head>
            <body>
                <div id="topmenu">
                    <div id="left">
                        <% if null resp then (show_all,[]) else (show_all,resp) %>
                        <p id="description"><% maybe (<% "No description." %>) (parsedDesc ids) desc %></p>
                    </div>
                    <div id="right">

                        <p id="view_plain"><a href=("/" ++ id)>Plain</a></p>

                        <form id="reply-form" method="post" action="/">
                            <input type="hidden" name="reply" value=id />
                            <input type="hidden" name="description" value=("Reply to /" ++ id ++ "/") />
                            <p id="reply"><a href="javascript:document.getElementById('reply-form').submit();">Reply</a></p>
                        </form>

                        <p id="available_languages">Available languages:</p>
                        <select size="1" id="languages" 
                          onchange="menu = document.getElementById('languages'); window.location = menu.options[menu.selectedIndex].text">
                            <%
                                map langOptions ("Text" : languages)
                            %>
                        </select>

                    </div>
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
        shortDesc = let sd = take 30 $ fromMaybe "" desc
                    in if (Just sd) == desc
                          then sd
                          else sd ++ "..."

-- | Parse a description and generate its EmbedAsChild list
parsedDesc :: (EmbedAsChild m String, EmbedAsAttr m (Attr [Char] [Char]))
           => [ID]
           -> String
           -> GenChildList m
parsedDesc ids desc = let descVals = PPD.parseDesc desc
                          unpack (PPD.Text t)                   = <% t %>
                          unpack (PPD.Username u)               = <% u %>
                          unpack (PPD.Tag t)                    = <% t %>
                          unpack (PPD.ID i) | (ID i) `elem` ids = <% let id' = "/" ++ i ++ "/" in <a class="link-to-id" href=id'><% id' %></a> %>
                                            | otherwise         = <% "/" ++ i ++ "/" %>
                      in <% [<% "Description: " %>] ++ map unpack descVals %>




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
              <td class="lineNumbers">
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

-- | EmbedAsChild instance for (Bool,[ID]), where Bool is whether all replies
-- should be shown or not and [ID] is the list of replies.
instance (XMLGenerator m, EmbedAsChild m XML) => (EmbedAsChild m (Bool,[ID])) where
    asChild (_,[])  = <% <p id="responses">No replies.</p> %>
    asChild (False,ids) | length ids > 10 =
        <%
            <p id="responses">Replies: <%
                (map (\(ID i) -> let id = "/" ++ i ++ "/" in <a href=id><% id %></a>) $ take 10 ids)
                ++ [<a href="?replies=all">(more)</a>]
            %></p>
        %>
    asChild (_,ids) =
        <%
            <p id="responses">Replies: <%
                map (\(ID i) -> let id = "/" ++ i ++ "/" in <a href=id><% id %></a>) ids
            %></p>
        %>
