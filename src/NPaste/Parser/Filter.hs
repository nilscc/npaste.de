{-# OPTIONS -fno-warn-unused-do-bind #-}

module NPaste.Parser.Filter
  ( parseFilter
  , filterToSearch
  ) where

import Control.Applicative
import Data.Char
import Data.Maybe
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))

import NPaste.Types.Parser.Filter
import NPaste.Types.Search
import NPaste.Parser.Description (tagSpecialChars)
import NPaste.Utils hiding (choice)

--------------------------------------------------------------------------------
-- Parser

parseFilter :: String -> Either ParseError Filter
parseFilter = parse parseF "filter"

parseF :: Parser Filter
parseF = choice [parseId, {- parseUName, -} parseTag, parseDesc, parseLang]
         `sepEndBy`
         many1 (space <?> "")

-- parse different DescVals:
parseId, {- parseUName, -} parseTag, parseLang, parseDesc :: Parser FilterVal

parseId = do
  char '/' 
  f <- fmap FilterID $ many1 (alphaNum <?> "a valid ID, ended by a '/'")
  char '/' <?> ""
  return f
parseDesc = do
  char '"'
  f <- fmap FilterDescription $ manyTill anyChar (lookAhead (char '"' <?> "a \" to end the description"))
  char '"'
  return f
-- parseUName = do
  -- char '@' 
  -- fmap FilterUsername $ many1 (alphaNum <?> "a valid username")
parseTag  = char '#' *> (fmap FilterTag      $ many1 ((alphaNum <|> oneOf tagSpecialChars) <?> "a valid tag"))
parseLang =             (fmap FilterLanguage $ many1 (satisfy (not . isSpace)))


-- | Constructing `Search` request from a `Filter`
filterToSearch :: Filter -> Maybe Search
filterToSearch [] = Nothing
filterToSearch f  = Just $ foldr1 S_And $ map toS f
 where
  toS fval = case fval of
                  FilterID          i  -> S_Or (S_PasteId i) $ S_Or (S_ReplyTo i) (S_ReplyOf i)
                  FilterUsername    u  -> S_UserName u
                  FilterTag         t  -> S_Tag t
                  FilterDescription d  -> S_PasteDesc d
                  FilterLanguage    l
                    | l == "Plaintext" -> S_PasteType Nothing
                    | otherwise        -> S_PasteType $ findLang l <|> Just l
