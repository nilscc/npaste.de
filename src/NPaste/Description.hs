module NPaste.Description
    ( parseDesc
    , DescVal (..)
    , tagsOnly
    , idsOnly
    , usernamesOnly
    ) where

import Control.Applicative
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

-- Elements
data DescVal = ID String (Maybe String)
             | Username String
             | Tag String
             | Text String
             deriving Show

--------------------------------------------------------------------------------
-- DescVal selectors

tagsOnly :: [DescVal] -> [String]
tagsOnly vals = [ t | Tag t <- vals ]

idsOnly :: [DescVal] -> [(String, Maybe String)]
idsOnly vals = [ (i,mu) | ID i mu <- vals ]

usernamesOnly :: [DescVal] -> [String]
usernamesOnly vals = [ u | Username u <- vals ]


--------------------------------------------------------------------------------
-- Parser

-- | Parse a Description, use "/id/" for a Paste ID, "@user" für user reference
-- and "#happstack" for tags
parseDesc :: String -> [DescVal]
parseDesc = either (const []) id . parse parseAll "description"

-- parse different DescVals:
pasteId, userName, tag, descVal :: Parser DescVal

pasteId  = try $ char '/' *> fmap newId    (many1 alphaNum) <* char '/'
 where newId i = ID i Nothing
userName = try $ char '@' *> fmap Username (many1 alphaNum)
tag      = try $ char '#' *> fmap Tag      (many1 alphaNum)

descVal = pasteId <|> userName <|> tag

-- put everything together
parseAll :: Parser [DescVal]
parseAll = concatDV <$> (descVal  <|> Text `fmap` pure `fmap` anyChar)
                    <*> (parseAll <|> return [])

  where concatDV (Text a)       (Text b     : rest) =
          Text (a++b) : rest
        concatDV (ID i Nothing) (Username u : rest) =
          ID i (Just u) : rest
        concatDV a rest = a : rest
