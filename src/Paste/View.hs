module Paste.View (showPaste) where

import Happstack.Server
import Happstack.State

import Text.Highlighting.Kate
import qualified Text.XHtml as X
import Text.XHtml
    ( (+++)
    , (<<)
    , (!)
    )

import Control.Monad (msum, mzero)
import Control.Monad.Trans (liftIO)

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
    mPaste <- query . GetPasteById . ID $ id
    case mPaste of
         Nothing -> notFound . toResponse $ "Paste not found with ID \"" ++ id ++ "\""
         Just p  -> do
             msum [ path $ showWithSyntax p
                  , trailingSlash  >> showWithSyntax p "hs"
                  , showPlain p
                  ]

-- | Simple method to get the content string of a PasteEntry
getContent :: PasteEntry -> IO String
getContent p = case content p of
                    Plain text -> return text
                    File file  -> readFile file

-- | Syntax highlighting
showWithSyntax :: PasteEntry -> String -> ServerPartT IO Response
showWithSyntax p ext = do
    code <- liftIO $ getContent p
    case highlightAs ext code of
         Left _       -> mzero
         Right result -> let css = X.style ! [X.thetype "text/css"] $ X.primHtml defaultHighlightingCss

                             pageTitle = X.thetitle << "Paste on n-sch.de"
                             metadata = X.meta ! [ X.httpequiv "Content-Type"
                                                 , X.content "text/html; charset=UTF-8"
                                                 , X.name "generator", X.content "paste.n-sch.de"
                                                 ]
                             style = X.itag "link" ! [ X.href "/static/style.css"
                                                     , X.thetype "text/css"
                                                     , X.rel "stylesheet"
                                                     ]
                             srcHtml = formatAsXHtml [OptNumberLines] ext $ result
                             topDiv = X.thediv ! [X.strAttr "id" "topmenu"] << (X.p << "Available languages:" +++ theLanguages)
                             theLanguages = X.select ! [ X.size "1"
                                                       , X.strAttr "id" "languages"
                                                       , X.strAttr "onChange" $
                                                           "menu = document.getElementById('languages');" ++
                                                           "window.location = menu.options[menu.selectedIndex].text"
                                                       ] << map (X.option <<) (("Current: " ++ lang) : languages)
                             lang = case languagesByExtension ext of
                                         (first : _) -> first
                                         _           -> ext


                             html = X.header << [pageTitle, metadata, css, style] +++ X.body << [topDiv, srcHtml]
                         in ok . toResponse $ html

showPlain :: PasteEntry -> ServerPartT IO Response
showPlain p = liftIO (getContent p) >>= ok . setHeader "Content-Type" "text/plain" . toResponse
