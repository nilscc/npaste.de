module Paste.Parser.Description
    ( parseDesc
    , DescVal (..)
    ) where

import Control.Applicative
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

-- Our data structure
data DescVal = ID String
             | Username String
             | Tag String
             | Text String
             deriving Show

-- | Parse a Description, use "/id/" for a Paste ID, "@user" für user reference
-- and "#happstack" for tags
parseDesc :: String -> [DescVal]
parseDesc = either (const []) id . parse parseAll "description"


-- parse different DescVals:
pasteId, userName, tag, descVal :: Parser DescVal

pasteId  = try $ char '/' *> ID       `fmap` many1 alphaNum <* char '/'
userName = try $ char '@' *> Username `fmap` many1 alphaNum
tag      = try $ char '#' *> Tag      `fmap` many1 alphaNum

descVal = pasteId <|> userName <|> tag

-- put everything together
parseAll :: Parser [DescVal]
parseAll = concatText <$> (descVal  <|> Text `fmap` pure `fmap` anyChar)
                      <*> (parseAll <|> return [])

  where concatText (Text a) (Text b : rest) = Text (a++b) : rest
        concatText a rest = a : rest
