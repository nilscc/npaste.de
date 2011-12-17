module NPaste.Types.Parser.Filter
  ( Filter
  , FilterVal (..)
  ) where

type Filter = [FilterVal]

data FilterVal
  = FilterID          String
  | FilterUsername    String
  | FilterTag         String
  | FilterLanguage    String
  | FilterDescription String
  deriving (Eq, Show)
