module NPaste.Utils.Highlight
  ( highlightAs
  , findLang
  , languages
  ) where

import Data.ByteString (ByteString)
-- import Data.Char
-- import Data.List
-- import Data.Text.Encoding (decodeUtf8With)
-- import Data.Text.Encoding.Error (ignore)

-- import Control.Applicative
-- import Control.Monad

import Text.Blaze.Html
-- import qualified Text.Blaze.Html5             as H
-- import qualified Text.Blaze.Html5.Attributes  as A
-- import Text.Highlighter

highlightAs :: String       -- ^ language
            -> ByteString   -- ^ source
            -> Maybe Html
highlightAs _lang _src = do -- maybe monad
  -- lexr <- findLexer lang
  -- toks <- maybeLeft (runLexer lexr src)
  -- Just (highlight toks)
  Nothing
 --where
  --maybeLeft = either (const Nothing) Just

{-
-- stolen from Text.Highlighter.Formatters.Html :)
highlight :: [Token] -> Html
highlight [] = return ()
highlight (Token t s:ts) = do
  H.span ! A.class_ (toValue $ shortName t) $
    toHtml (decodeUtf8With ignore s)
  highlight ts

findLexer :: String -> Maybe Lexer
findLexer lang =
  getFirst [ snd <$> find matches lexers
           , lexerFromFilename lang ]
 where
  getFirst = msum
  matches (e,l) = or
    [ map toLower (lName l) == map toLower lang
    , drop 1 e == map toLower lang ]
-}

findLang :: String -> Maybe String
findLang lang = do
  --lexr <- findLexer lang
  --Just (lName lexr)
  Just lang

languages :: [String]
--languages = nub (map (lName . snd) lexers)
languages = []
