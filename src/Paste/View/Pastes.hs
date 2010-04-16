{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses,
    UndecidableInstances, FlexibleContexts, TypeFamilies,
    NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}

module Paste.View.Pastes
    ( showPaste
    , Description (..)
    ) where

import Happstack.Server
import Happstack.State

import HSP
import Text.Highlighting.Kate
import qualified HSX.XMLGenerator   as HSX
import Control.Monad
import Control.Monad.Trans
import Text.Pandoc                  (readMarkdown, defaultParserState, ParserState (..))

import Data.Char
import Data.Maybe
import Data.List                    (find, nub)
import qualified Data.Set as S

import App.View
import Paste.State
import qualified Paste.Parser.Description as PPD


--------------------------------------------------------------------------------
-- Get data, handle events
--------------------------------------------------------------------------------

-- | Variation of showPaste which accepts multiple IDs
showPaste :: String -> ServerPart Response
showPaste ids = do
    pastes <- mapM (query . GetPasteById . ID) $ words ids
    case sequence pastes of
         Nothing   -> notFound . toResponse $ "Paste not found with ID \"" ++ ids ++ "\""
         Just list -> msum [ path $ \ext -> showWithSyntax $ map (\p -> p { filetype = PFileType $ Just ext }) list
                           , trailingSlash >> showWithSyntax (nub list)
                           , showPlain $ head list
                           ]


-- | Simple method to get the content string of a PasteEntry
getContent :: PasteEntry -> IO PContent
getContent p = case content p of
                    plain@(PContent (Plain _)) -> return plain
                    (PContent (File file))     -> readFile file >>= return . PContent . Plain

-- | Show plain text
showPlain :: PasteEntry -> ServerPart Response
showPlain p = do
    (PContent (Plain text)) <- liftIO $ getContent p
    ok . setHeader "Content-Type" "text/plain" . toResponse $ text

-- | Syntax highlighting
showWithSyntax :: [PasteEntry] -> ServerPart Response
showWithSyntax p = showWithSyntax' p >>= xmlResponse

showWithSyntax' :: [PasteEntry] -> ServerPart HtmlBody
showWithSyntax' [] = return $
    HtmlBody [ WithCss $ CssString defaultHighlightingCss
             , WithCss $ CssFile "/static/css/paste.css"
             ]
             [ ]
showWithSyntax' (p:ps) = do

        let id = unPId $ pId p

        ids     <- query $ GetAllIds
        replies <- query $ GetAllReplies id

        showAllReplies <- getDataQueryFn $ look "replies"

        cont <- liftIO $ getContent p

        let
            shortDesc = case unPDescription $ description p of
                             Nothing -> ""
                             Just d | length d > 30 -> ": " ++ take 30 d ++ "..."
                                    | otherwise     -> ": " ++ d

            htmlOpts = [ WithTitle $ "npaste.de - Paste #" ++ unId id ++ shortDesc ]

            pview = PasteView { showAll = showAllReplies == Just "all"
                              , allIds  = ids
                              , viewReplies = replies
                              , pasteEntries = [p { content = cont }]
                              }

        (HtmlBody opts xmls) <- showWithSyntax' ps
        return $ HtmlBody (htmlOpts ++ opts) ([<div><% pview %></div>] ++ xmls)


--------------------------------------------------------------------------------
-- Data stuff
--------------------------------------------------------------------------------

-- | Options for the 
data PasteView = PasteView { showAll :: Bool
                           , allIds  :: S.Set ID
                           , viewReplies :: [ID]
                           , pasteEntries :: [PasteEntry]
                           }

data Description = Description (S.Set ID) String


--------------------------------------------------------------------------------
-- XML instances
--------------------------------------------------------------------------------

-- | XML generator for PasteEntry
instance (XMLGenerator m, EmbedAsChild m XML, HSX.XML m ~ XML) => (EmbedAsChild m PasteView) where
    asChild pview@(PasteView { pasteEntries = pes }) =
        <% map `flip` pes $ \ PasteEntry { content = content'
                                         , filetype = filetype'
                                         , description = description'
                                         , pId = pId'
                                         } ->
            let
                content     = case unPContent content' of
                                   Plain text -> text
                                   _ -> ""
                description = unPDescription description'
                -- filetype    = unPFileType filetype'
                pId         = unPId pId'
                id          = unId pId
                -- parsedDesc  = PPD.parseDesc $ fromMaybe "" description

                langOptions l
                    | l == language = <option selected="selected"><% l ++ " (current)" %></option>
                    | otherwise     = <option><% l %></option>

                language    = language' $ fromMaybe "" (unPFileType filetype')

                language' s
                    | s `elem` ["Render Markdown", "render-markdown"] = "Render Markdown"
                language' s =
                    case find ((map toLower s ==) . map toLower) languages of
                         Just l -> l
                         _ | not (null $ languagesByExtension s) -> head $ languagesByExtension s
                           | otherwise                           -> "Text"

            in
            <%
                [
                    <div class=("topmenu")>
                        <div class="left">
                            <p class="replies"><a href=("/" ++ id ++ "/")>/<% id %>/</a> - <% (showAll pview, viewReplies pview) %></p>
                            <p class="description"><% Description (allIds  pview) $ fromMaybe "" description %></p>
                        </div>
                        <div class="right">

                            -- View plain button
                            <p class="view_plain"><a href=("/" ++ id)>Plain</a></p>

                            -- Reply button
                            <form id=("reply-form-" ++ id) class="reply-form" method="post" action="/">
                                <input type="hidden" name="reply" value=id />
                                <input type="hidden" name="description" value=("Reply to /" ++ id ++ "/") />
                                <p class="reply"><a href=("javascript:document.getElementById('reply-form-" ++ unId pId ++ "').submit();")>Reply</a></p>
                            </form>

                            -- Language selection
                            <p class="available_languages">Available languages:</p>
                            <select size="1" class="languages" id=("languages-" ++ id)
                              onchange=("menu = document.getElementById('languages-" ++ id ++ "'); window.location = menu.options[menu.selectedIndex].text")>
                                <%
                                    map langOptions ("Text" : "Render Markdown" : languages)
                                %>
                            </select>

                        </div>
                    </div>
                ]
                ++ case language of
                        "Render Markdown" ->
                            [
                                <div class="renderedText">
                                    <% pandocToXml $ readMarkdown defaultParserState { stateSanitizeHTML = True } content %>
                                </div>
                            ]
                        _                 ->
                            [
                                <table class="sourceCode">
                                    <tr>
                                        <td class="lineNumbers">
                                            <pre><%
                                                let clickable n = <a href=("#n" ++ n) class="no" name=("n" ++ n) id=("n" ++ n)><% n ++ "\n" %></a>
                                                in map clickable . map (fst) $ zip (map show [(1 :: Int)..]) (lines content)
                                            %></pre>
                                        </td>
                                        <td class="sourceCode">
                                            <%
                                                case highlightAs language content of
                                                     Left _   -> <pre><% "\n" ++ content %></pre>
                                                     Right sl -> htmlToXml $ formatAsXHtml [] language sl
                                            %>
                                        </td>
                                    </tr>
                                </table>
                            ]
            %>
        %>


instance (XMLGenerator m) => (EmbedAsChild m Description) where
    asChild (Description _ "") =
        <%
            [<% "No description." %>]
        %>
    asChild (Description ids desc) =
        let
            descVals = PPD.parseDesc desc
            unpack (PPD.Text t)                   = <% t %>
            unpack (PPD.Username u)               = <% u %>
            unpack (PPD.Tag t)                    = <% t %>
            unpack (PPD.ID i) | (ID i) `S.member` ids = <% let id' = "/" ++ i ++ "/" in <a class="link-to-id" href=id'><% id' %></a> %>
                              | otherwise             = <% "/" ++ i ++ "/" %>
        in
        <%
            [<% "Description: " %>] ++ map unpack descVals
        %>



-- | EmbedAsChild instance for (Bool,[ID]), where Bool is whether all replies
-- should be shown or not and [ID] is the list of replies.
instance (XMLGenerator m) => (EmbedAsChild m (Bool,[ID])) where
    asChild (_,[])  = <% "No replies." %>
    asChild (False,ids) | length ids > 10 =
        <%
            [<% "Replies: " %>]
            ++ map (\(ID i) -> let id = "/" ++ i ++ "/" in <% <a href=id><% id %></a> %>) (take 10 ids)
            ++ [<% <a href="?replies=all">(more)</a> %>]
        %>
    asChild (_,ids) =
        <%
            [<% "Replies: " %>] ++ map (\(ID i) -> let id = "/" ++ i ++ "/" in <% <a href=id><% id %></a> %>) ids
        %>
