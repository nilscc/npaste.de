{-# LANGUAGE OverloadedStrings, ViewPatterns, NamedFieldPuns #-}
{-# OPTIONS -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

module NPaste.Html.Read where

import Data.ByteString.Char8 (unpack)
import Data.List                    as L
import Data.Time
import Data.Maybe
import System.Locale
import Text.Blaze.Html5 (toHtml, toValue, (!))
import qualified Text.Blaze.Html5             as H
import qualified Text.Blaze.Html5.Attributes  as A

import Text.Highlighting.Kate

import NPaste.Types

-- | Show links in description etc
formatDesc :: Description -> Html
formatDesc d =
  sequence_ . for d $ \dval ->
    case dval of
         DescText     t -> toHtml t
         DescUsername u -> H.a ! A.href (toValue $ "/v/user/" ++ u) ! A.class_ "descUser" $ toHtml ("@" ++ u)
         DescTag      t -> H.a ! A.href (toValue $ "/v/tag/"  ++ t) ! A.class_ "descTag"  $ toHtml ("#" ++ t)
         DescID       i -> H.a ! A.href (toValue $ "/" ++ i ++ "/") ! A.class_ "descID"   $ toHtml ("/" ++ i ++ "/")
 where
  for = flip L.map

--------------------------------------------------------------------------------
-- | Read/show a single paste
readHtml :: Maybe Paste -> Html
readHtml (Just p@Paste{ pasteType = Just lang, pasteContent }) = do
  H.div ! A.class_ "formatedCode" $ do
    let cont = unpack pasteContent
    case highlightAs lang cont of
         Right source -> formatCode p source
         Left  err    -> do
           H.p ! A.class_ "warning" $ toHtml err
           formatPlain p cont

readHtml (Just p@Paste{ pasteContent }) = do
  H.div ! A.class_ "formatedCode" $
    formatPlain p $ unpack pasteContent

readHtml _ = do
  H.h3 ! A.class_ "error" $ "Paste not found."

-- | Information for the compact header
readInfo :: Maybe Paste
         -> [Id]              -- ^ replies
         -> Html
readInfo Nothing  _ = do
  -- logo & description
  H.p ! A.id "logo" $ H.a ! A.href "/" $ do
    H.span ! A.id "n3" $ do
      "n"
      H.sup "3"
    "paste.de"
readInfo (Just p) r = do

  -- paste information
  H.div ! A.id "compactMenu" $
    H.div ! A.class_ "pasteInfo" $ do
      unless (null r) $ do
        H.p ! A.class_ "replies" $ do
          "Replies: "
          sequence_ . intersperse " " . for r $ \pid ->
            let url = "/" ++ pid ++ "/"
             in H.a ! A.href (toValue url) $ toHtml url
      H.form ! A.action "/" ! A.method "post" ! A.class_ "addReply" $ do
        H.input ! A.type_ "hidden" ! A.name "desc"    ! A.value (toValue $ "Reply to " ++ p_id)
        H.input ! A.type_ "hidden" ! A.name "hidden"  ! A.value (if pasteHidden p then "on" else "")
        H.input ! A.type_ "submit" ! A.name "asreply" ! A.value "New reply"
      H.p ! A.id "view_all_replies" $
        H.a ! A.href (toValue $ "/v/id" ++ p_id) $ "Show related"
      H.p ! A.class_ "timestamp" $
        toHtml $ formatTime defaultTimeLocale "%H:%M - %a %Y.%m.%d" (pasteDate p)
      H.form ! A.action (H.toValue p_id) ! A.method "post"
             ! A.class_ "languageSelector" $ do
        H.select ! A.id "lang" ! A.name "lang" $
          forM_ ("Plaintext" : languages) $ \l ->
            if Just l == pasteType p then -- TODO: different highlighting languages
              H.option ! A.selected "selected" ! A.value (toValue l) $ toHtml l
             else
              H.option                         ! A.value (toValue l) $ toHtml l
        H.input ! A.type_ "submit" ! A.value "Change language"

  -- logo & description
  H.p ! A.id "logo" $ H.a ! A.href "/" $ do
    H.span ! A.id "n3" $ do
      "n"
      H.sup "3"
    "paste.de"
  case pasteDescription p of
       Just d | not (null d) -> H.p ! A.class_ "desc" $ formatDesc d
       _                     -> return ()
 where
  for = flip L.map
  p_id = "/" ++ pasteId p ++ "/"

--------------------------------------------------------------------------------
-- | List multiple pastes
listPastes :: [Paste]
           -> Maybe Int     -- ^ number of lines (Nothing = no limit)
           -> Html
listPastes [] _                               = return ()
listPastes (p@Paste{ pasteContent } : r) lnum = do
  pasteInfo p
  H.div ! A.class_ "formatedCode" $ do
    let cont = maybe id (\i -> unlines . take i . lines) lnum $ unpack pasteContent
        lang = pasteType p
    case lang of
         Nothing -> formatPlain p cont
         Just l  ->
           case highlightAs l cont of
               Right source -> formatCode p source
               Left  err    -> do
                 H.p ! A.class_ "warning" $ toHtml err
                 formatPlain p cont
  listPastes r lnum

-- | Show a nice header with all kind of informations about our paste
pasteInfo :: Paste -> Html
pasteInfo Paste{ pasteId, pasteDate, pasteDescription, pasteType } =
  H.div ! A.class_ "pasteInfo" $ do
    H.p ! A.class_ "timestamp" $
      toHtml $ formatTime defaultTimeLocale "%H:%M - %a %Y.%m.%d" pasteDate
    H.p ! A.class_ "language" $ do
      let lang = case pasteType of
                      Nothing -> "Plaintext"
                      Just t  -> t
      H.a ! A.href (toValue $ "/v/lang/" ++ lang) $ toHtml lang
    H.p $ do
      "Paste: "
      H.a ! A.href (toValue $ "/" ++ pasteId ++ "/")
          $ toHtml ("/" ++ pasteId ++ "/")
      H.a ! A.href (toValue $ "/v/id/" ++ pasteId)
          $ "Show related"
    H.p ! A.class_ "desc" $
      case pasteDescription of
           Just d  -> formatDesc d
           Nothing -> "No description."

--------------------------------------------------------------------------------
-- Highlight source code

-- | Turn source lines into HTML
formatCode :: Paste
           -> [SourceLine]      -- ^ Source lines to format
           -> Html
formatCode Paste{pasteId} source = do
  H.div ! A.class_ "lineNumbers" $ H.pre ! A.class_ "lineNumbers" $
    sequence_ . intersperse H.br . for [1..length source] $ \(show->n) ->
      let name = "line-" ++ n
          url  = "/" ++ pasteId ++ "/#" ++ name
       in H.a ! A.href (toValue url) ! A.name (toValue name) $ toHtml n
  H.div ! A.class_ "sourceCode" $ H.pre ! A.class_ "sourceCode" $
    sequence_ . intersperse H.br $ L.map sourceLineToHtml source
 where
  for = flip L.map

sourceLineToHtml :: SourceLine -> Html
sourceLineToHtml []             = return ()
sourceLineToHtml (([],txt):r)   = toHtml txt >> sourceLineToHtml r
sourceLineToHtml ((labs,txt):r) = do
  let atts = drop 1 labs -- no idea why, lol
  if null atts
     then toHtml txt
     else H.span ! A.class_ (toValue $ intercalate " " atts) $ toHtml txt
  sourceLineToHtml r

-- | Format plain text without highlighting
formatPlain :: Paste -> String -> Html
formatPlain Paste{pasteId} cont = do
  let l = lines cont
  H.div ! A.class_ "lineNumbers" $ H.pre $
    sequence_ . intersperse H.br . for [1..length l] $ \(show->n) ->
      let name = "line-" ++ n
          url  = "/" ++ pasteId ++ "/#" ++ name
       in H.a ! A.href (toValue url) ! A.name (toValue name) $ toHtml n
  H.pre ! A.class_ "plainText" $ toHtml cont
 where
  for = flip L.map

