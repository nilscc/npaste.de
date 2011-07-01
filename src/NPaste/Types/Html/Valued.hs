module NPaste.Types.Html.Valued
  ( Valued (..)
  )  where

import Text.Blaze

class Valued a where
  (?) :: a -> String -> AttributeValue
  getValue :: a -> String -> String

  a ? k = toValue $ getValue a k
