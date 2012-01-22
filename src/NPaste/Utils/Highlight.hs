module NPaste.Utils.Highlight
  ( highlightAs
  , findLang
  , languages
  ) where

import Data.ByteString (ByteString)
import Data.Char
import Data.List

import Control.Applicative
import Control.Monad

import Text.Blaze
import Text.Highlighter
import Text.Highlighter.Formatters.Html

highlightAs :: String       -- ^ language
            -> ByteString   -- ^ source
            -> Maybe Html
highlightAs lang src = do -- maybe monad
  lexr <- findLexer lang
  toks <- maybeLeft (runLexer lexr src)
  Just (format False toks)
 where
  maybeLeft = either (const Nothing) Just

findLexer :: String -> Maybe Lexer
findLexer lang =
  getFirst [ snd <$> find matches lexers
           , lexerFromFilename lang ]
 where
  getFirst = msum
  matches (e,l) = or
    [ map toLower (lName l) == map toLower lang
    , drop 1 e == map toLower lang ]

findLang :: String -> Maybe String
findLang lang = do
  lexr <- findLexer lang
  Just (lName lexr)

languages :: [String]
languages = nub (map (lName . snd) lexers)
