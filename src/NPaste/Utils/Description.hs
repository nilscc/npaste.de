module NPaste.Utils.Description
    ( parseDesc
    , DescVal (..)
    , Description
      -- ** Description filters
    , tagsOnly
    , idsOnly
    , usernamesOnly
    ) where

import Control.Applicative
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

import NPaste.Types

--------------------------------------------------------------------------------
-- DescVal selectors

tagsOnly :: Description -> [String]
tagsOnly vals = [ t | DescTag t <- vals ]

idsOnly :: Description -> [(String, Maybe String)]
idsOnly vals = [ (i,mu) | DescID i mu <- vals ]

usernamesOnly :: Description -> [String]
usernamesOnly vals = [ u | DescUsername u <- vals ]


--------------------------------------------------------------------------------
-- Parser

-- | Parse a Description, use "/id/" for a Paste ID, "@user" für user reference
-- and "#happstack" for tags
parseDesc :: String -> Description
parseDesc = either (const []) id . parse parseAll "description"

-- parse different DescVals:
pasteId', userName, tag, descVal :: Parser DescVal

pasteId' = try $ char '/' *> fmap newId    (many1 alphaNum) <* char '/'
 where newId i = DescID i Nothing
userName = try $ char '@' *> fmap DescUsername (many1 alphaNum)
tag      = try $ char '#' *> fmap DescTag      (many1 alphaNum)

descVal = pasteId' <|> userName <|> tag

-- put everything together
parseAll :: Parser Description
parseAll = concatDV <$> (descVal  <|> DescText `fmap` pure `fmap` anyChar)
                    <*> (parseAll <|> return [])

  where concatDV (DescText a)       (DescText b     : rest) =
          DescText (a++b) : rest
        concatDV (DescID i Nothing) (DescUsername u : rest) =
          DescID i (Just u) : rest
        concatDV a rest = a : rest

--------------------------------------------------------------------------------
-- Printer

-- showDesc :: Description -> String
-- showDesc [] = ""
-- showDesc (ID id 
