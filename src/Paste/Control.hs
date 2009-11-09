module Paste.Control (pasteHandler) where

import Happstack.State
import Happstack.Server

-- import Paste.View
import Paste.State
import Users.State
    ( Validate (..)
    , UserReply (..)
    , Login (..)
    , Password (..)
    )
import App.Helper ( isDir )

import qualified Text.XHtml as X
import Text.XHtml
    ( (+++)
    , (<<)
    , (!)
    )

import Text.Highlighting.Kate

import Data.Char (isSpace)
import Data.Maybe (fromJust, isJust)
import Control.Monad (msum, mplus, mzero, guard)
import Control.Monad.Trans (liftIO)
import System.Time (getClockTime, ClockTime(..))
import System.IO.Unsafe (unsafePerformIO)

pasteHandler :: ServerPartT IO Response
pasteHandler = msum
    [ postHandler
    , dir "static" $ fileServe [] "public"
    , path showPaste
    , fileServe ["index.html"] "public"
    ]

-- {{{ Handle post events

-- | Remove any trailing white space characters
stripSpaces :: String -> String
stripSpaces = unlines . map (foldr strip "") . lines
  where strip s "" = if isSpace s then "" else [s]
        strip s r  = s : r


-- | Take care of POST events
-- TODO: add a port 8000 filter and dont allow 127.0.0.1 ips
postHandler :: ServerPartT IO Response
postHandler = methodM POST >> msum
    [ do
        -- Get content, asure its not empty
        paste <- getDataFn $ do
            s <- look "content"
            -- Limit size to 50k chars
            guard . null . drop (50000) $ s
            guard . not . null . filter (not . isSpace) $ s
            return . stripSpaces $ s
        guard $ isJust paste

        -- Get login data
        login <- getDataFn $ do
            u <- look "user"
            p <- look "pwd"
            return (u,p)
        user <- case login of
                     Just (user,pwd) -> do
                         resp  <- query $ Validate (Login user) (Password pwd)
                         return $ case resp of
                                       OK user -> Just user
                                       _       -> Nothing
                     _ -> return $ Nothing

        -- Local time
        now   <- liftIO getClockTime

        -- New ID, filepath and content text
        newId <- query $ GetNewId
        let fp   = "pastes/" ++ unId newId
            text = fromJust paste
        
        -- Save the content to file
        liftIO $ writeFile fp text

        update $ AddPaste PasteEntry { date    = Just now
                                     , content = File fp
                                     , user    = user
                                     , pId     = Nothing
                                     }

        -- Handle redirection from a html form
        let url = "http://npaste.de/" ++ unId newId

        submit <- getDataFn $ look "submit"
        if isJust submit
           then seeOther url $ toResponse url
           else ok . toResponse $ url ++ "\n"
    , badRequest . toResponse $ "Something went wrong. Contact mail (at) n-sch.de if necessary.\n"
    ]

-- }}} Handle post events

-- | Show paste handler
showPaste :: String -> ServerPartT IO Response
showPaste id = do
    mPaste <- query . GetPasteById . ID $ id
    case mPaste of
         Nothing -> notFound . toResponse $ "Paste not found with ID \"" ++ id ++ "\""
         Just p  -> do
             msum [ path $ showWithSyntax p
                  , isDir >> showWithSyntax p "hs"
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
