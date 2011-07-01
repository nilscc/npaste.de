module NPaste.Types.Html.Index where

import NPaste.Types.Html.Valued

newtype IndexPostData = IndexPostData [(String, Either FilePath String)]
  deriving (Eq, Show)

instance Valued IndexPostData where
  getValue (IndexPostData lis) k = case lookup k lis of
    Just (Right val) -> val
    _               -> ""
