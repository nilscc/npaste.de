module Paste.Parser.Description
    ( parseDesc
    , DescVal (..)
    ) where

import Text.ParserCombinators.Parsec

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

pasteId = try $ do
    char '/'
    id <- many1 alphaNum
    char '/'
    return $ ID id

userName = try $ do
    char '@'
    un <- many1 alphaNum
    return $ Username un

tag = try $ do
    char '#'
    t <- many1 alphaNum
    return $ Tag t

descVal = pasteId <|> userName <|> tag


-- put everything together
parseAll :: Parser [DescVal]
parseAll = do
    val  <- descVal <|> (anyChar >>= return . Text . (:[]))
    rest <- parseAll <|> return []
    return $ case val : rest of
                  (Text a : Text b : rest) -> Text (a++b) : rest
                  other -> other
