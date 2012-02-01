module NPaste.Types.Html.PostData where

import Data.Text.Encoding
import Data.Text.Encoding.Error
import Data.ByteString (ByteString, empty)
import Text.Blaze

newtype PostData = PostData [(String, Either FilePath ByteString)]
  deriving (Eq, Show)

class Valued a where
  (?)      :: a -> String -> AttributeValue
  getValue :: a -> String -> ByteString

  a ? k = toValue . decodeUtf8With ignore $ getValue a k

instance Valued PostData where
  getValue (PostData lis) k = case lookup k lis of
    Just (Right val) -> val
    _                -> empty
