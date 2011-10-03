{-# LANGUAGE OverloadedStrings, ViewPatterns, NamedFieldPuns #-}
{-# OPTIONS -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

module NPaste.Html.Read where

import Data.ByteString.Char8 (ByteString, unpack)
import Data.List                    as L
import Data.Time
import System.Locale
import Text.Blaze.Html5             as H
import Text.Blaze.Html5.Attributes  as A

import Text.Highlighting.Kate

import NPaste.Types
import NPaste.Utils

-- | Show links in description etc
formatDesc :: String -> Html
formatDesc d =
  sequence_ . for (parseDesc d) $ \dval ->
    case dval of
         DescText     t -> toHtml t
         DescUsername u -> H.a ! A.href (toValue $ "/u/" ++ u) ! A.class_ "descUser" $ toHtml u
         DescTag      t -> toHtml t -- TODO
         DescID    i mu -> let url = maybe ("/" ++ i ++ "/") (\u -> "/u/" ++ u ++ "/" ++ i ++ "/") mu
                            in H.a ! A.href (toValue url) ! A.class_ "descID" $ toHtml url
 where
  for = flip L.map

--------------------------------------------------------------------------------
-- | Read/show a single paste
readHtml :: Maybe PasteInfo -> Maybe ByteString -> Html
readHtml (Just p@PasteInfo{ p_type = Just lang}) (Just cont) = do
  H.div ! A.class_ "formatedCode" $ do
    let cont' = unpack cont
    case highlightAs lang cont' of
         Right source -> formatCode p source
         Left  err    -> do
           H.p ! A.class_ "warning" $ toHtml err
           formatPlain p cont'

readHtml (Just p) (Just cont) = do
  H.div ! A.class_ "formatedCode" $
    formatPlain p $ unpack cont

readHtml _ _ = do
  H.h3 ! A.class_ "error" $ "Paste not found."

-- | Information for the compact header
readInfo :: Maybe PasteInfo
         -> [ID]              -- ^ replies
         -> Html
readInfo Nothing  _ = return ()
readInfo (Just p) r = do
  H.div ! A.class_ "pasteInfo" $ do
    unless (null r) $ H.p ! A.class_ "replies" $ do
      "Replies: "
      sequence_ . intersperse " " . for r $ \pId ->
        let url = case pId of
                       ID pId'                     -> "/" ++ pId' ++ "/"
                       PrivateID User{u_name} pId' -> "/u/" ++ u_name ++ "/" ++ pId' ++ "/"
         in H.a ! A.href (toValue url) $ toHtml url
    H.form ! A.action "/" ! A.method "post" ! A.class_ "addReply" $ do
      H.input ! A.type_ "hidden" ! A.name "desc" ! A.value (toValue $ "Reply to /" ++ p_id p ++ "/")
      H.input ! A.type_ "submit" ! A.name "asreply" ! A.value "New reply"
    H.p ! A.class_ "timestamp" $
      toHtml $ formatTime defaultTimeLocale "%H:%M - %a %Y.%m.%d" (p_date p)
    H.form ! A.class_ "languageSelector" ! A.method "post" $ do
      H.select ! A.id "lang" ! A.name "lang" $
        forM_ ("Plain text" : languages) $ \l ->
          if Just l == p_type p then -- TODO
            H.option ! A.selected "selected" ! A.value (toValue l) $ toHtml l
           else
            H.option                         ! A.value (toValue l) $ toHtml l
      H.input ! A.type_ "submit" ! A.value "Change language" ! A.name "submit"
  case p_description p of
       Just d | not (null d) -> H.p ! A.class_ "desc" $ formatDesc d
       _                     -> return ()
 where
  for = flip L.map

--------------------------------------------------------------------------------
-- | Show recent Pastes
recentHtml :: [(PasteInfo, Maybe ByteString)] -> Html
recentHtml []               =
  return ()
recentHtml ((p, Just c):r)  = do
  recentInfo p
  H.div ! A.class_ "formatedCode" $ do
    let cont = unlines . take 20 . lines $ unpack c
        lang = p_type p
    case lang of
         Nothing -> formatPlain p cont
         Just l  ->
           case highlightAs l cont of
               Right source -> formatCode p source
               Left  err    -> do
                 H.p ! A.class_ "warning" $ toHtml err
                 formatPlain p cont
  recentHtml r
recentHtml ((p, Nothing):r) = do
  recentInfo p
  H.h3 ! A.class_ "error" $ "Content not found/invalid."
  recentHtml r

-- | Show a nice header with all kind of informations about our paste
recentInfo :: PasteInfo -> Html
recentInfo PasteInfo{ p_id, p_date, p_description, p_type } =
  H.div ! A.class_ "pasteInfo" $ do
    H.p ! A.class_ "timestamp" $
      toHtml $ formatTime defaultTimeLocale "%H:%M - %a %Y.%m.%d" p_date
    H.p ! A.class_ "language" $
      case p_type of
           Nothing -> "Plain text"
           Just t  -> toHtml t
    H.p $ do
      "Paste: "
      H.a ! A.href (toValue $ "/" ++ p_id) $
        toHtml $ "/" ++ p_id ++ "/"
    H.p ! A.class_ "desc" $
      case p_description of
           Just d  -> formatDesc d
           Nothing -> "No description."

--------------------------------------------------------------------------------
-- Highlight source code

-- | Turn source lines into HTML
formatCode :: PasteInfo
           -> [SourceLine]      -- ^ Source lines to format
           -> Html
formatCode PasteInfo{p_id} source = do
  H.div ! A.class_ "lineNumbers" $ H.pre $
    sequence_ . intersperse br . for [1..length source] $ \(show->n) ->
      let name = "line-" ++ n
          url  = "/" ++ p_id ++ "#" ++ name
       in H.a ! A.href (toValue url) ! A.name (toValue name) $ toHtml n
  H.div ! A.class_ "sourceCode" $
    H.pre $ sequence_ . intersperse br $ L.map sourceLineToHtml source
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
formatPlain :: PasteInfo -> String -> Html
formatPlain PasteInfo{p_id} cont = do
  let l = lines cont
  H.div ! A.class_ "lineNumbers" $ H.pre $
    sequence_ . intersperse br . for [1..length l] $ \(show->n) ->
      let name = "line-" ++ n
          url  = "/" ++ p_id ++ "#" ++ name
       in H.a ! A.href (toValue url) ! A.name (toValue name) $ toHtml n
  H.pre ! A.class_ "plainText" $ toHtml cont
 where
  for = flip L.map

