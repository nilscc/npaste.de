module NPaste.Parser.Description
    ( -- * Parse descriptions
      parseDesc
    , DescVal (..)
    , Description
    , descToString
      -- ** Description filters
    , tagsOnly
    , idsOnly
    , usernamesOnly
      -- ** Validation
    , validTag
    , tagSpecialChars
    ) where

import Control.Applicative
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

import NPaste.Types.Parser.Description

--------------------------------------------------------------------------------
-- DescVal selectors

tagsOnly :: Description -> [String]
tagsOnly vals = [ t | DescTag t <- vals ]

idsOnly :: Description -> [String]
idsOnly vals = [ i | DescID i <- vals ]

usernamesOnly :: Description -> [String]
usernamesOnly vals = [ u | DescUsername u <- vals ]

descToString :: Description -> String
descToString (DescText     t:r) =        t ++        descToString r
descToString (DescTag      t:r) = "#" ++ t ++        descToString r
descToString (DescUsername u:r) = "@" ++ u ++        descToString r
descToString (DescID       i:r) = "/" ++ i ++ "/" ++ descToString r
descToString []                 = ""

--------------------------------------------------------------------------------
-- Parser

-- | Parse a Description, use "/id/" for a Paste ID, "@user" für user reference
-- and "#happstack" for tags
parseDesc :: String -> Description
parseDesc = either (const []) id . parse parseAll "description"

-- parse different DescVals:
pasteId', userName, tag, descVal :: Parser DescVal

pasteId' = try $ char '/' *> fmap DescID       (many1 alphaNum) <* char '/'
userName = try $ char '@' *> fmap DescUsername (many1 alphaNum)
tag      = try $ char '#' *> fmap DescTag      (many1 $ alphaNum <|> oneOf tagSpecialChars)

descVal = pasteId' <|> userName <|> tag

tagSpecialChars :: [Char]
tagSpecialChars = "!\"§$%&()=?`´}][{³²+*~#'÷-_…·<>|^°"

validTag :: String -> Bool
validTag [] = False
validTag s  = all (`elem` (['A'..'Z'] ++ ['a'..'z'] ++ tagSpecialChars)) s


-- put everything together
parseAll :: Parser Description
parseAll = concatDV <$> (descVal  <|> DescText `fmap` pure `fmap` anyChar)
                    <*> (parseAll <|> return [])

  where concatDV (DescText a)       (DescText b     : rest) =
          DescText (a++b) : rest
        -- concatDV (DescID i Nothing) (DescUsername u : rest) =
          -- DescID i (Just u) : rest
        concatDV a rest = a : rest

--------------------------------------------------------------------------------
-- Printer

-- showDesc :: Description -> String
-- showDesc [] = ""
-- showDesc (ID id 
