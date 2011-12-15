module NPaste.Types.Description
  ( Description
  , DescVal (..)
  ) where

type Description = [DescVal]

-- Elements
data DescVal = DescID String -- (Maybe String)
             | DescUsername String
             | DescTag String
             | DescText String
             deriving (Eq, Show)
