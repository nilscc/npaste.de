module NPaste.Types.Html.PostData where

import Text.Blaze

newtype PostData = PostData [(String, Either FilePath String)]
  deriving (Eq, Show)

class Valued a where
  (?)      :: a -> String -> AttributeValue
  getValue :: a -> String -> String

  a ? k = toValue $ getValue a k

instance Valued PostData where
  getValue (PostData lis) k = case lookup k lis of
    Just (Right val) -> val
    _                -> ""
