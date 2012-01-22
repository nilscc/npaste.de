module NPaste.Utils.Kate
  ( findLang
  ) where

import Data.Char
import Data.Maybe
import Text.Highlighting.Kate

findLang :: String -> Maybe String
findLang lang = listToMaybe $
  [ l | l <- "Plaintext" : languages
      , map toLower lang == map toLower l ]
  ++
  languagesByExtension lang
  ++
  languagesByFilename lang
