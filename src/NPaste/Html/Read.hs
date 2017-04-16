{-# LANGUAGE OverloadedStrings, ViewPatterns, NamedFieldPuns #-}
{-# OPTIONS -fno-warn-name-shadowing -fno-warn-unused-do-bind #-}

module NPaste.Html.Read where

import Data.List                              as L
import Data.Time
import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE
import qualified Data.Text.Encoding.Error     as TEE
import Data.Maybe
import Text.Blaze.Html5 (toHtml, toValue, (!))
import qualified Text.Blaze.Html5             as H
import qualified Text.Blaze.Html5.Attributes  as A
import qualified Data.ByteString.Char8        as B8

import NPaste.Types
import NPaste.Utils

decodeUtf8 :: B8.ByteString -> Text
decodeUtf8 = TE.decodeUtf8With TEE.ignore

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
readHtml (Just Paste{ pasteId, pasteType = Just lang, pasteContent }) = do
  H.div ! A.class_ "formatedCode" $
    formatCode pasteId lang (decodeUtf8 pasteContent)

readHtml (Just Paste{ pasteId, pasteContent }) = do
  H.div ! A.class_ "formatedCode" $
    formatPlain pasteId $ decodeUtf8 pasteContent

readHtml _ = do
  H.h3 ! A.class_ "error" $ "Paste not found."

-- | Information for the compact header
readInfo :: Maybe Paste
         -> Maybe User
         -> [Id]              -- ^ replies
         -> Html
readInfo Nothing _ _ = do
  -- logo & description
  H.p ! A.id "logo" $ H.a ! A.href "/" $ do
    H.span ! A.id "n3" $ do
      "n"
      H.sup "3"
    "paste.de"
readInfo (Just p) mu r = do

  H.div ! A.class_ "compact-menu" $ do

    -- list all replies
    unless (null r) $ do
      H.p ! A.class_ "replies" $ do
        "Replies: "
        sequence_ . intersperse " " . for r $ \pid ->
          let url = "/" ++ pid ++ "/"
           in H.a ! A.href (toValue url) $ toHtml url

    H.div ! A.class_ "paste-functions" $ do
      -- "New reply" button
      H.form ! A.action "/" ! A.method "post" ! A.class_ "addReply" $ do
        H.input ! A.type_ "hidden" ! A.name "desc"    ! A.value (toValue $ "Reply to " ++ p_id)
        H.input ! A.type_ "hidden" ! A.name "hidden"  ! A.value (if pasteHidden p then "on" else "")
        H.input ! A.type_ "submit" ! A.name "asreply" ! A.value "New reply"
      -- more functions (popout menu)
      H.ul ! A.class_ "functions" $ do
        -- "Show related" link
        H.li $
          H.a ! A.href (toValue $ "/v/id" ++ p_id) $ "Show related"
        -- User profile link
        maybe (return ()) `flip` mu $ \User{ userName } ->
          H.li $ H.a ! A.href (toValue $ "/u/profile/" ++ userName) $
            toHtml $ "Profile: " ++ userName


    -- timestamp
    H.p ! A.class_ "timestamp" $
      toHtml $ formatTime defaultTimeLocale "%H:%M - %a %Y.%m.%d" (pasteDate p)

    -- language selector
    H.div ! A.class_ "language-selector" $ do
      H.p $ toHtml $ ("Language: " ++) . fromMaybe "Plaintext" $
                     join . fmap findLang $ pasteType p
      H.ul $ do
        H.li $ H.a ! A.href (toValue p_id)                  $ "Default language"
        H.li $ H.a ! A.href (toValue $ p_id ++ "Plaintext") $ "Plaintext"
        H.li ! A.class_ "spacer" $ H.hr
        forM_ (languages) $ \l ->
          H.li $ H.a ! A.href (toValue $ p_id ++ escape l) $ toHtml l

  -- logo
  H.p ! A.id "logo" $ H.a ! A.href "/" $ do
    H.span ! A.id "n3" $ do
      "n"
      H.sup "3"
    "paste.de"

  -- description
  case pasteDescription p of
       Just d | not (null d) -> H.p ! A.class_ "desc" $ formatDesc d
       _                     -> return ()

 where
  for = flip L.map
  p_id = "/" ++ pasteId p ++ "/"

  escape = concatMap $ \c -> case c of
    ' '  -> "%20"
    '+'  -> "%2B"
    '<'  -> "%3C"
    '>'  -> "%3E"
    '#'  -> "%23"
    '%'  -> "%25"
    '{'  -> "%7B"
    '}'  -> "%7D"
    '|'  -> "%7C"
    '\\' -> "%5C"
    '^'  -> "%5E"
    '~'  -> "%7E"
    '['  -> "%5B"
    ']'  -> "%5D"
    '`'  -> "%60"
    ';'  -> "%3B"
    '/'  -> "%2F"
    '?'  -> "%3F"
    ':'  -> "%3A"
    '@'  -> "%40"
    '&'  -> "%26"
    '$'  -> "%24"
    ok   -> [ok]

--------------------------------------------------------------------------------
-- | List multiple pastes
listPastes :: [Paste]
           -> Maybe Int     -- ^ number of lines (Nothing = no limit)
           -> Html
listPastes [] _                               = return ()
listPastes (p@Paste{ pasteId, pasteContent } : r) lnum = do
  pasteInfo p
  H.div ! A.class_ "formatedCode" $ do
    let preview = maybe id (\i -> B8.unlines . take i . B8.lines) lnum $ pasteContent
        cont = TE.decodeUtf8With TEE.ignore preview
        lang = pasteType p
    case lang of
         Nothing -> formatPlain pasteId cont
         Just _l  ->
           -- TODO: re-enable highlighting
           --case highlightAs l preview of
               --Just html -> formatCode  p{ pasteContent = preview} html
               --Nothing   ->
           formatPlain pasteId cont
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

-- HTML for line numbers
lineNumbers
  :: Id           -- ^ PasteID
  -> Int          -- ^ Number of lines
  -> Maybe String -- ^ Language to link to
  -> Html
lineNumbers pasteId numberOfLines mLang =
  H.div ! A.class_ "lineNumbers" $ H.pre ! A.class_ "lineNumbers" $
    sequence_ . intersperse H.br . for [1..numberOfLines] $ \(show->n) ->
      let url  = "/p/" ++ pasteId ++ "/" ++ fromMaybe "" mLang ++ "#" ++ n
       in H.a ! A.href (toValue url) ! A.name (toValue n) $ toHtml n
 where
  for = flip L.map

-- | Turn source lines into HTML
formatCode
  :: Id     -- ^ Paste ID
  -> String -- ^ Paste language
  -> Text   -- ^ Paste content
  -> Html
formatCode pasteId lang cont = do
  let numberOfLines = length $ T.lines cont
  lineNumbers pasteId numberOfLines (Just lang)
  H.div ! A.class_ "sourceCode" $
    H.pre $ H.code ! A.class_ (toValue $ "lang-" ++ filter isAlphaNum lang) $
      toHtml cont

-- | Format plain text without highlighting
formatPlain :: Id -> Text -> Html
formatPlain pasteId cont = do
  let numberOfLines = length $ T.lines cont
  lineNumbers pasteId numberOfLines Nothing
  H.pre ! A.class_ "plainText" $ toHtml cont
